# If you're reading this, please contact Sebastiaan Joosten
# Reason: this script was made for a single setup
#    it is probably not suitable for your machine
#    remainder of these comments are for my future self

# Idea: check where this makefile is run
# if on server, install cgi
# if local, sync with server and call 'make' on server

# directory that must exist server-side (for establishing that we are on the server)
homedir = /home/sjc/
# directory that must exist developer-side (for establishing that we are on the developer code)
devdir = /Users/sjc/
# server address from the client side
server = f004d0r@www.cs.dartmouth.edu
# remaining directories are server side:
# installation directory for cgi scripts
installdir = $(homedir)public_html/cs30/
maincgi = $(installdir)cs30.cgi
testcgi = $(installdir)test.cgi
srcdir = src/
staticdir = static/
tesths = $(srcdir)test.hs
mainhs = $(srcdir)Main.hs
datadir = $(homedir)cs30data/
hidir = .tmp

choose : $(homedir) $(devdir)

$(devdir) :
	@echo "We may be running on server, attempt cgi script install"
	@$(MAKE) all

$(homedir) :
	@echo "We may be running on dev machine, attempt sync"
	@$(MAKE) sync

all : checkserver cgi dir
.PHONY : checkdev sync

# run a check to ensure we're on the dev machine
checkdev :
	@file $(devdir) > /dev/null

# run a check to ensure we're on the server
checkserver :
	@file $(homedir) > /dev/null

sync : checkdev
	@echo "Synchronising code with server:"
	rsync -varz --exclude=".?*" --delete-after . $(server):cs30
	@echo "\nBuilding code server-side:"
	ssh -t $(server) "cd cs30/&&make all"

# directory structure
dir : checkserver $(installdir) $(hidir)

# cgi executables
cgi : $(testcgi) $(maincgi)

$(hidir) :
	mkdir -p $(hidir)

# not used currently
$(datadir) :
	mkdir -p $(datadir)
	chmod 711 $(datadir)

$(installdir) : $(shell find $(staticdir) -type f -name '*')
	mkdir -p $(installdir)
	chmod 755 $(installdir)
	echo "WARNING: files in this directory are generated automatically and may be overwritten." > $(installdir)WARNING.md
	cp -R $(staticdir)* $(installdir)

$(testcgi) : dir $(tesths)
	@ghc --make $(tesths) -hidir=$(hidir) -odir=$(hidir) -o $(testcgi)
	@chmod 755 $(testcgi)

$(maincgi) : dir $(mainhs) $(datadir)
	printf "$(datadir)" > data/dir
	@~/.local/bin/stack install --local-bin-path=$(installdir)
	@chmod 755 $(maincgi)

# Q: Why is there no make clean?
# I put in absolute paths to install scripts on the server.
# If I allowed 'clean' that removes all created files,
# there would be a chance of taking the server down inadvertently.
# Moreover, there is no safe way to remove the directory itself.
# Deleting $(installdir) should be done manually.
