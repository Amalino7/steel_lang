/// Simple string similarity module for suggesting corrections in error messages.
/// Uses Levenshtein distance to find similar names.
///
/// Computes the Levenshtein distance between two strings
fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_len = a.len();
    let b_len = b.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row = vec![0; b_len + 1];

    for (i, a_char) in a.chars().enumerate() {
        curr_row[0] = i + 1;

        for (j, b_char) in b.chars().enumerate() {
            let cost = if a_char == b_char { 0 } else { 1 };
            curr_row[j + 1] = (curr_row[j] + 1)
                .min(prev_row[j + 1] + 1)
                .min(prev_row[j] + cost);
        }

        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// Finds similar strings from a list of candidates
/// Returns up to `max_suggestions` similar strings, sorted by similarity
///
/// Heuristic:
/// 1. Distance is <= 3
/// 2. Distance is <= half the target length
/// 3. Starts with the same length and close distance
pub fn find_similar<'a>(
    target: &str,
    candidates: impl IntoIterator<Item = &'a str>,
    max_suggestions: usize,
) -> Vec<String> {
    let target_lower = target.to_lowercase();
    let mut similarities: Vec<(usize, &str)> = candidates
        .into_iter()
        .filter_map(|candidate| {
            let candidate_lower = candidate.to_lowercase();
            let distance = levenshtein_distance(&target_lower, &candidate_lower);

            let max_distance = 3.min(target.len() / 2 + 1);
            if distance <= max_distance
                || (target_lower.chars().next() == candidate_lower.chars().next()
                    && distance <= target.len() / 2)
            {
                Some((distance, candidate))
            } else {
                None
            }
        })
        .collect();

    similarities.sort_by_key(|(dist, _)| *dist);

    similarities
        .into_iter()
        .take(max_suggestions)
        .map(|(_, name)| name.to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("", ""), 0);
        assert_eq!(levenshtein_distance("abc", "abc"), 0);
        assert_eq!(levenshtein_distance("abc", "abd"), 1);
        assert_eq!(levenshtein_distance("abc", ""), 3);
        assert_eq!(levenshtein_distance("", "abc"), 3);
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
    }

    #[test]
    fn test_find_similar() {
        let candidates = ["name", "age", "address", "email", "phone"];

        // Typo: naem -> name
        let result = find_similar("naem", candidates.iter().copied(), 3);
        assert!(result.contains(&"name".to_string()));
        assert!(result.len() <= 3);

        // Typo: adress -> address
        assert_eq!(
            find_similar("adress", candidates.iter().copied(), 3),
            vec!["address"]
        );

        // Multiple matches, sorted by similarity
        let result = find_similar("ame", candidates.iter().copied(), 3);
        assert!(result.contains(&"name".to_string()));

        // No close matches
        let result = find_similar("xyz", candidates.iter().copied(), 3);
        assert!(result.is_empty());
    }
}
