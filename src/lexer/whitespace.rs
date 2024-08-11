// unicode format-control https://tc39.es/ecma262/#sec-unicode-format-control-characters
/// ZERO WIDTH NO-BREAK SPACE
pub(super) const ZWNBSP: char = '\u{FEFF}';

// white space https://tc39.es/ecma262/#sec-white-space
/// CHARACTER TABULATION
pub(super) const TAB: char = '\u{0009}';
/// LINE TABULATION
pub(super) const VT: char = '\u{000B}';
/// FORM FEED
pub(super) const FF: char = '\u{000C}';
/// NO-BREAK SPACE
pub(super) const NBSP: char = '\u{00A0}';
/// SPACE
pub(super) const SPACE: char = '\u{0020}';

/// character is whitespace according to the specification https://tc39.es/ecma262/#sec-white-space
pub fn is_spec_whitespace(ch: char) -> bool {
    // the named characters are mentioned in the spec, the rest are in the unicode "Space Separator"
    // category: https://www.compart.com/en/unicode/category/Zs
    ch == TAB
        || ch == VT
        || ch == FF
        || ch == ZWNBSP
        || ch == SPACE
        || ch == NBSP
        || ch == '\u{1680}'
        || ch == '\u{2000}'
        || ch == '\u{2001}'
        || ch == '\u{2002}'
        || ch == '\u{2003}'
        || ch == '\u{2004}'
        || ch == '\u{2005}'
        || ch == '\u{2006}'
        || ch == '\u{2007}'
        || ch == '\u{2008}'
        || ch == '\u{2009}'
        || ch == '\u{200A}'
        || ch == '\u{202F}'
        || ch == '\u{205F}'
        || ch == '\u{3000}'
}

// line terminators https://tc39.es/ecma262/#sec-line-terminators
/// LINE FEED
pub(super) const LF: char = '\u{000A}';
/// CARRIAGE RETURN
pub(super) const CR: char = '\u{000D}';
/// LINE SEPARATOR
pub(super) const LS: char = '\u{2028}';
/// PARAGRAPH SEPARATOR
pub(super) const PS: char = '\u{2029}';

pub fn is_line_terminator(ch: char) -> bool {
    ch == LF || ch == CR || ch == LS || ch == PS
}
