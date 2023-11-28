all: SHP

Lexer.hs : Lexer.x
	alex -g Lexer.x

Parser.hs : Parser.y
	happy -gc Parser.y
    
SHP : Lexer.hs Parser.hs SHP.hs
	stack ghc -- --make SHP
    
clean:
	rm -f SHP Lexer.hs Parser.hs *.o *.hi
    
