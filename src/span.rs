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

    /// Creates a new [Span] starting at `start` with length of `size` bytes.
    ///
    /// # Example
    /// ```
    /// use rust_lox::span::Span;
    /// let span = Span::sized(4, 2);
    /// assert_eq!(span, Span::new(4, 6));
    /// ```
    #[inline]
    pub const fn sized(start: u32, size: u32) -> Self {
        Self::new(start, start + size)
    }

    #[inline]
    pub const fn len(&self) -> u32 {
        self.end - self.start
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Creates a new [Span] that has its start and end positions shrunk by
    /// `offset` amount.
    ///
    /// # Example
    /// ```
    /// use rust_lox::span::Span;
    /// let span = Span::new(4, 10);
    /// assert_eq!(span.shrink(2), Span::new(6, 8));
    /// ```
    pub fn shrink(self, offset: u32) -> Self {
        let start = self.start + offset;
        let end = self.end - offset;
        debug_assert!(start <= end, "Cannot shrink span past zero length");
        Self::new(start, end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<(usize, usize)> for Span {
    #[inline]
    fn from(value: (usize, usize)) -> Self {
        Self::new(value.0 as u32, value.1 as u32)
    }
}

impl From<Span> for Range<usize> {
    #[inline]
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        // Unfortunately miette uses usize for spans
        (span.start as usize, span.len() as usize).into()
    }
}

/// Enables `&input[span]`
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
        // (usize, usize) -> Span
        let input: (usize, usize) = (2, 8);
        let span: Span = input.into();
        assert_eq!(span, Span::new(2, 8));

        // Span -> Range<usize>
        let range: Range<usize> = Span::new(2, 8).into();
        assert_eq!(range, 2..8);

        // Span -> miette::SourceSpan
        let source_span: miette::SourceSpan = Span::new(2, 8).into();
        assert_eq!(source_span, miette::SourceSpan::new(2.into(), 6));
    }

    #[test]
    fn test_slice_string() {
        let input = "foo bar baz";
        let span = Span::new(4, 7);

        assert_eq!(&input[span], "bar");
        assert_eq!(input.get(Range::<usize>::from(span)), Some("bar"));
    }
}
