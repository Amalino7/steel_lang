use crate::vm::value::{Closure, Function, Value};
use std::alloc::{GlobalAlloc, Layout, System};
use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
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

// Cannot guarantee that this won't lead to multiple mutable borrows.
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

pub struct GarbageCollector {
    next_gc: usize,
    objects: Vec<NonNull<HeapHeader>>,
    gray_stack: VecDeque<NonNull<HeapHeader>>,
}

impl GarbageCollector {
    const GROWTH_FACTOR: usize = 2;

    pub fn new() -> Self {
        Self {
            next_gc: 1024 * 1024,
            objects: Vec::with_capacity(1024),
            gray_stack: VecDeque::new(),
        }
    }

    pub fn collect(&mut self) {
        #[cfg(feature = "debug_gc")]
        let start_size = self.allocated;

        self.trace_ref();
        self.sweep();

        #[cfg(feature = "debug_gc")]
        println!("GC Collected: {} bytes", start_size - self.allocated);

        if ALLOCATED.load(Relaxed) > self.next_gc {
            self.grow();
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
        self.gray_stack.push_back(header);
    }

    pub fn mark_value(&mut self, val: &Value) {
        match val {
            Value::Number(_) => {}
            Value::String(str) => {
                self.mark(*str);
            }
            Value::Function(gc) => {
                self.mark(*gc);
            }
            Value::NativeFunction(_) => {}
            Value::Boolean(_) => {}
            Value::Nil => {}
            Value::Closure(closure) => {
                self.mark(*closure);
            }
        }
    }

    pub fn should_collect(&self) -> bool {
        #[cfg(feature = "stress_gc")]
        return true;

        #[cfg(not(feature = "stress_gc"))]
        return ALLOCATED.load(Relaxed) > self.next_gc;
    }

    fn grow(&mut self) {
        self.next_gc *= Self::GROWTH_FACTOR;
    }

    fn trace_ref(&mut self) {
        while let Some(ptr) = self.gray_stack.pop_back() {
            unsafe {
                let header = ptr.as_ref();
                if header.is_marked {
                    continue;
                }
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
        for constant in self.chunk.constants.iter() {
            gc.mark_value(constant)
        }
    }
}

impl Trace for Closure {
    fn trace(&self, gc: &mut GarbageCollector) {
        gc.mark(self.function);
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
