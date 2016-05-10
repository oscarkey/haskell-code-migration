clean:
	rm -f build/*
	rm -f Listen
	rm -f MigratingGetFile

listen: clean
	ghc -ieffect-handlers:src/library -odir build -hidir build -o Listen ./src/library/Listen.hs	

getfile: clean
	ghc -ieffect-handlers:src/library -odir build -hidir build -o MigratingGetFile ./src/examples/getfile/MigratingGetFile.hs

all: listen getfile
