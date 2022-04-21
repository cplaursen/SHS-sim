all: SHP

SHPLexer.hs : SHPLexer.x
	alex SHPLexer.x

SHPParser.hs : SHPParser.y
	happy SHPParser.y
    
SHP : SHPLexer.hs SHPParser.hs SHP.hs
	ghc --make SHP
    
clean:
	rm -f SHP SHPLexer.hs SHPParser.hs *.o *.hi
    
