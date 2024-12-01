use std::{
    fmt::Display,
    ops::{Index, IndexMut, Range},
};

/// A range in text, represented by a zero-indexed start and end byte offset.
///
/// It is a logical error if `end` is less than `start`.
///
/// It uses [u32] for offsets instead of [usize] to reduce memory footprint, though
/// the trade-off is that only files up to 4GB are supported
/// ([reference](https://oxc.rs/docs/learn/performance.html#span)).
///
/// # Example
///
/// ```
/// use rust_lox::span::Span;
///
/// let input = "foo bar baz";
/// let span = Span::new(4, 7);
/// assert_eq!(&input[span], "bar");
/// ```
///
/// # Implementation
///
/// Heavily inspired by [`oxc_span`](https://github.com/oxc-project/oxc/blob/main/crates/oxc_span/src/span/mod.rs).
#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct Span {
    /// The zero-based start offset of the span
    pub start: u32,
    /// The zero-based end offset of the span. This may be equal to [`start`](Span::start) if
    /// the span is empty, but should not be less than it.
    pub end: u32,
}

impl Span {
    #[inline]
    pub const fn new(start: u32, end: u32) -> Self {
        Span { start, end }
    }

    #[inline]
    pub const fn len(&self) -> u32 {
        self.end - self.start
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Range<u32>> for Span {
    #[inline]
    fn from(range: Range<u32>) -> Self {
        Self::new(range.start, range.end)
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        // Unfortunately miette uses usize for spans
        (span.start as usize, span.len() as usize).into()
    }
}

impl Index<Span> for str {
    type Output = str;

    #[inline]
    fn index(&self, index: Span) -> &Self::Output {
        &self[index.start as usize..index.end as usize]
    }
}

impl IndexMut<Span> for str {
    #[inline]
    fn index_mut(&mut self, index: Span) -> &mut Self::Output {
        &mut self[index.start as usize..index.end as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_conversion() {
        // Range -> Span
        let span: Span = (2..8).into();
        assert_eq!(span, Span::new(2, 8));

        // Span -> miette::SourceSpan
        let source_span: miette::SourceSpan = Span::new(2, 8).into();
        assert_eq!(source_span, miette::SourceSpan::new(2.into(), 6));
    }

    #[test]
    fn test_index() {
        let input = "foo bar baz";
        let span = Span::new(4, 7);
        assert_eq!(&input[span], "bar");
    }
}
