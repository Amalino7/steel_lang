use crate::vm::value::{
    BoundMethod, Closure, EnumVariant, Function, Instance, InterfaceObj, List, VTable, Value,
};
use std::alloc::{GlobalAlloc, Layout, System};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::Relaxed;

/// Trait for types that can be traced by the garbage collector
pub trait Trace {
    fn trace(&self, gc: &mut GarbageCollector);
}

struct HeapHeader {
    trace_fn: unsafe fn(*mut u8, &mut GarbageCollector),
    drop_fn: unsafe fn(*mut u8),
    is_marked: bool,
}

#[repr(C)]
struct GcBox<T: ?Sized> {
    header: HeapHeader,
    value: T,
}

pub struct Gc<T: ?Sized + Trace + 'static> {
    ptr: NonNull<GcBox<T>>,
}

impl<T: Trace> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T: Trace> Copy for Gc<T> {}

impl<T: Trace> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        *self
    }
}

/// Cannot guarantee that this won't lead to multiple mutable borrows.
impl<T: Trace> Gc<T> {
    pub(crate) unsafe fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut self.ptr.as_mut().value }
    }
}

impl<T: Trace> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.ptr.as_ref().value }
    }
}

impl<T: Trace> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Gc({:?})", self.ptr)
    }
}
impl<T: Trace> Display for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ptr)
    }
}

pub struct GarbageCollector {
    next_gc: usize,
    live_bytes_after_gc: usize,
    objects: Vec<NonNull<HeapHeader>>,
    gray_stack: Vec<NonNull<HeapHeader>>,
}

impl GarbageCollector {
    const THRESHOLD_GC: usize = 1024 * 16; //16KB Allocated after last GC
    pub fn new() -> Self {
        Self {
            next_gc: Self::THRESHOLD_GC,
            objects: Vec::with_capacity(1024),
            gray_stack: Vec::with_capacity(100),
            live_bytes_after_gc: 0,
        }
    }

    pub fn collect(&mut self) {
        self.trace_ref();
        #[cfg(not(feature = "stress_gc"))]
        let size_before_gc = ALLOCATED.load(Relaxed);
        self.sweep();
        #[cfg(not(feature = "stress_gc"))]
        {
            let live = ALLOCATED.load(Relaxed);
            let survival_ratio = live as f64 / (size_before_gc as f64);
            if survival_ratio > 0.8 {
                self.next_gc *= 2;
            } else if survival_ratio < 0.3 {
                self.next_gc = (self.next_gc / 2).max(Self::THRESHOLD_GC);
            }

            self.live_bytes_after_gc = live;
        }
    }

    pub fn alloc<T: Trace + 'static>(&mut self, value: T) -> Gc<T> {
        unsafe fn trace<T: Trace + 'static>(ptr: *mut u8, marker: &mut GarbageCollector) {
            unsafe {
                let ptr = ptr.cast::<GcBox<T>>();
                (*ptr).value.trace(marker);
            }
        }

        unsafe fn drop<T: Trace + 'static>(ptr: *mut u8) {
            unsafe {
                let ptr = ptr.cast::<GcBox<T>>();
                let _ = Box::from_raw(ptr);
            }
        }

        let gc_box = Box::new(GcBox {
            header: HeapHeader {
                is_marked: false,
                trace_fn: trace::<T>,
                drop_fn: drop::<T>,
            },
            value,
        });

        let ptr = NonNull::new(Box::into_raw(gc_box)).unwrap();

        self.objects.push(ptr.cast());

        Gc { ptr }
    }

    /// Entry point for the VM to mark roots (Stack, Globals)
    pub fn mark<T: Trace + 'static>(&mut self, ptr: Gc<T>) {
        let header = ptr.ptr.cast::<HeapHeader>();
        unsafe {
            if (*header.as_ptr()).is_marked {
                return;
            }
        }
        self.gray_stack.push(header);
    }

    pub fn mark_value(&mut self, val: Value) {
        match val {
            Value::Number(_) => {}
            Value::String(str) => {
                self.mark(str);
            }
            Value::Function(gc) => {
                self.mark(gc);
            }
            Value::NativeFunction(_) => {}
            Value::Boolean(_) => {}
            Value::Nil => {}
            Value::Closure(closure) => {
                self.mark(closure);
            }
            Value::Instance(instance) => {
                self.mark(instance);
            }
            Value::BoundMethod(bound_method) => {
                self.mark(bound_method);
            }
            Value::InterfaceObj(interface_obj) => {
                self.mark(interface_obj);
            }
            Value::Enum(enum_variant) => {
                self.mark(enum_variant);
            }
            Value::List(list) => {
                self.mark(list);
            }
        }
    }

    pub fn should_collect(&self) -> bool {
        #[cfg(feature = "stress_gc")]
        return true;

        #[cfg(not(feature = "stress_gc"))]
        {
            ALLOCATED.load(Relaxed) > self.next_gc + self.live_bytes_after_gc
        }
    }

    fn trace_ref(&mut self) {
        while let Some(ptr) = self.gray_stack.pop() {
            unsafe {
                let header = ptr.as_ref();
                (*ptr.as_ptr()).is_marked = true;
                (header.trace_fn)(ptr.as_ptr() as *mut u8, self);
            }
        }
    }

    fn sweep(&mut self) {
        self.objects.retain(|ptr| unsafe {
            let header = ptr.as_ref();
            if header.is_marked {
                (*ptr.as_ptr()).is_marked = false;
                true
            } else {
                (header.drop_fn)(ptr.as_ptr() as *mut u8);
                false
            }
        });
    }
}

// Clean-up on Drop
impl Drop for GarbageCollector {
    fn drop(&mut self) {
        for ptr in &self.objects {
            unsafe {
                let header = ptr.as_ref();
                let drop = header.drop_fn;
                drop(ptr.as_ptr() as *mut u8);
            }
        }
        self.objects.clear();
    }
}

impl Trace for String {
    fn trace(&self, _: &mut GarbageCollector) {}
}
impl Trace for Function {
    fn trace(&self, gc: &mut GarbageCollector) {
        for &constant in self.chunk.constants.iter() {
            gc.mark_value(constant)
        }
    }
}

impl Trace for Closure {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.mark(self.function);
        for &capture in self.captures.iter() {
            gc.mark_value(capture)
        }
    }
}

impl Trace for Instance {
    fn trace(&self, gc: &mut GarbageCollector) {
        for &field in self.fields.iter() {
            gc.mark_value(field);
        }
        gc.mark_value(self.name);
    }
}

impl Trace for BoundMethod {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.mark(self.method);
        gc.mark_value(self.receiver);
    }
}

impl Trace for InterfaceObj {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.mark_value(self.data);
        gc.mark(self.vtable);
    }
}

impl Trace for VTable {
    fn trace(&self, gc: &mut GarbageCollector) {
        for method in self.methods.iter() {
            gc.mark(*method);
        }
    }
}

impl Trace for EnumVariant {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.mark_value(self.enum_name);
        gc.mark_value(self.payload);
    }
}
impl Trace for List {
    fn trace(&self, gc: &mut GarbageCollector) {
        for e in self.vec.iter() {
            gc.mark_value(*e);
        }
    }
}

#[cfg(not(feature = "stress_gc"))]
struct Counter;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);

#[cfg(not(feature = "stress_gc"))]
unsafe impl GlobalAlloc for Counter {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ret = unsafe { System.alloc(layout) };
        if !ret.is_null() {
            ALLOCATED.fetch_add(layout.size(), Relaxed);
        }
        ret
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe {
            System.dealloc(ptr, layout);
        }
        ALLOCATED.fetch_sub(layout.size(), Relaxed);
    }
}

#[cfg(not(feature = "stress_gc"))]
#[global_allocator]
static A: Counter = Counter;
