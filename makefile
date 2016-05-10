gethandlers:
	if [ ! -d "effect-handlers" ]; then git clone https://github.com/slindley/effect-handlers.git; fi

clean:
	rm -f build/*
	rm -f Listen
	rm -f MigratingGetFile

listen: gethandlers clean
	ghc -ieffect-handlers:src/library -odir build -hidir build -o Listen ./src/library/Listen.hs	

getfile: gethandlers clean
	ghc -ieffect-handlers:src/library -odir build -hidir build -o MigratingGetFile ./src/examples/getfile/MigratingGetFile.hs

all: listen getfile
