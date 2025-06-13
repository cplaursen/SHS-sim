from pygments.lexer import RegexLexer
from pygments.token import Text, Keyword, Name, String, Number, Operator, Comment


class SHSLexer(RegexLexer):
    name = "SHS"
    aliases = ["shs"]
    filenames = ["*.shs"]

    tokens = {
        "root": [
            (r"#.*$", Comment.Single),  # Comments
            (
                r"\b(SHP|Variables|Constants|Enums)\b",
                Keyword.Namespace,
            ),  # Block Keywords
            (
                r"\b(if|else|while|skip|dt|dW|input)\b",
                Keyword.Reserved,
            ),  # Control flow and functions
            (r"\b(true|false)\b", Keyword.Constant),  # Constants
            (r"\b(real|bool|int|enum)\b", Keyword.Type),  # Type Keywords
            (r"\:=", Operator),  # Assignment operator
            (r"[-+*/=<>&]", Operator),  # Other operators
            (r"\b[0-9]+(\.[0-9]+)?\b", Number),  # Numbers
            (r'"[^"]*"', String),  # Strings
            (r"[a-zA-Z_][a-zA-Z0-9_]*\'?", Name),  # Identifiers
            (r"[{}();,]", Text),  # Structural symbols
            (r"\s+", Text),  # Whitespace
        ]
    }
