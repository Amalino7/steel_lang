use crate::vm::tests::helpers::*;

// --- sort_by ---

#[test]
fn test_sort_by_empty_list() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [].sort_by(less_than);
        assert(result.len(), 0);
        "#,
    );
}

#[test]
fn test_sort_by_single_element() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [42].sort_by(less_than);
        assert(result.len(), 1);
        assert(result[0], 42);
        "#,
    );
}

#[test]
fn test_sort_by_two_elements_ordered() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [1, 2].sort_by(less_than);
        assert(result.len(), 2);
        assert(result[0], 1);
        assert(result[1], 2);
        "#,
    );
}

#[test]
fn test_sort_by_two_elements_reversed() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [2, 1].sort_by(less_than);
        assert(result.len(), 2);
        assert(result[0], 1);
        assert(result[1], 2);
        "#,
    );
}

#[test]
fn test_sort_by_already_sorted() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [1, 2, 3, 4, 5].sort_by(less_than);
        assert(result.len(), 5);
        assert(result[0], 1);
        assert(result[1], 2);
        assert(result[2], 3);
        assert(result[3], 4);
        assert(result[4], 5);
        "#,
    );
}

#[test]
fn test_sort_by_reverse_sorted() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [5, 4, 3, 2, 1].sort_by(less_than);
        assert(result.len(), 5);
        assert(result[0], 1);
        assert(result[1], 2);
        assert(result[2], 3);
        assert(result[3], 4);
        assert(result[4], 5);
        "#,
    );
}

#[test]
fn test_sort_by_all_duplicates() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [7, 7, 7, 7].sort_by(less_than);
        assert(result.len(), 4);
        assert(result[0], 7);
        assert(result[1], 7);
        assert(result[2], 7);
        assert(result[3], 7);
        "#,
    );
}

#[test]
fn test_sort_by_general_unsorted() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let result = [10, 3, 2, 4, 1, 11, 45, 23].sort_by(less_than);
        assert(result.len(), 8);
        assert(result[0], 1);
        assert(result[1], 2);
        assert(result[2], 3);
        assert(result[3], 4);
        assert(result[4], 10);
        assert(result[5], 11);
        assert(result[6], 23);
        assert(result[7], 45);
        "#,
    );
}

#[test]
fn test_sort_by_does_not_mutate_original() {
    assert_runs(
        r#"
        func less_than(a: number, b: number): boolean { return a < b; }
        let original = [3, 1, 2];
        let sorted = original.sort_by(less_than);
        // original must be unchanged
        assert(original[0], 3);
        assert(original[1], 1);
        assert(original[2], 2);
        // sorted must be in order
        assert(sorted[0], 1);
        assert(sorted[1], 2);
        assert(sorted[2], 3);
        "#,
    );
}

#[test]
fn test_sort_by_descending_comparator() {
    assert_runs(
        r#"
        func greater_than(a: number, b: number): boolean { return a > b; }
        let result = [3, 1, 4, 1, 5, 9, 2, 6].sort_by(greater_than);
        assert(result.len(), 8);
        assert(result[0], 9);
        assert(result[1], 6);
        assert(result[2], 5);
        assert(result[3], 4);
        assert(result[4], 3);
        assert(result[5], 2);
        assert(result[6], 1);
        assert(result[7], 1);
        "#,
    );
}
