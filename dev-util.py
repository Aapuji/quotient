# Get all given keywords into `$keyword:lower => TokenKind::$keyword` format.
def generate_keyword_lines(keywords: str):
    words = [x.strip() for x in keywords.strip().split(',')]

    for word in words:
        print(f'"{word.lower()}" => TokenKind::{word},')


generate_keyword_lines("""    Let, Mut, And, Type, Class, Module, Impl, Deriving, Import, As,
    If, Then, Else, Match, With, Do, End, Using, Matches,
    Rec, Proc, Fun, Sealed, Extends, Some, 
    Prefix, Postfix, LAssoc, RAssoc, WithPrec, Lazy, Memo,""")
