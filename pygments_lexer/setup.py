from setuptools import setup

setup(
    name='pygments-shs-lexer',
    version='1.0',
    py_modules=['shs_lexer'],
    entry_points={
        'pygments.lexers': [
            'shs = shs_lexer:SHSLexer',
        ],
    },
)

