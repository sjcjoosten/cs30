# If you're reading this, please contact Sebastiaan Joosten
# Reason: this script was made for a single setup
#    it is probably not suitable for your machine
#    remainder of these comments are for my future self

# Idea: check where this makefile is run
# if on server, install cgi
# if local, sync with server and call 'make' on server

# directory that must exist server-side only (for establishing that we are on the server)
homedir = /home/sjc/
# directory that must exist developer-side only (for establishing that we are on the developer code)
devdir = /Users/sjc/
# server address from the client side
server = f004d0r@www.cs.dartmouth.edu
# remaining directories are server side:
# installation directory for cgi scripts
installdir = $(homedir)public_html/cs30/
maincgi = $(installdir)cs30.cgi
srcdir = cgi/
staticdir = static/
mainhs = $(srcdir)CGI.hs
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
	@test -e $(devdir) || (echo "This makefile is intended to help Sebastiaan install his software on a server, you should call 'stack' directly instead, per the README instructions" && exit 1)

# run a check to ensure we're on the server
checkserver :
	@test -e $(homedir)

sync : checkdev
	@echo "Synchronising code with server (ensure you are connected via VPN!):"
	rsync -varz --exclude=".?*" --exclude="data" --exclude="cs30.cabal" --delete-after . $(server):cs30
	@echo "\nBuilding code server-side:"
	ssh -t $(server) "cd cs30/&&make all"

# directory structure
dir : checkserver $(installdir) $(hidir)

# cgi executables
cgi : $(maincgi)

$(hidir) :
	mkdir -p $(hidir)

$(datadir) :
	mkdir -p $(datadir)
	chmod 711 $(datadir)

$(installdir) : $(shell find $(staticdir) -type f -name '*')
	mkdir -p $(installdir)
	chmod 755 $(installdir)
	echo "WARNING: files in this directory are generated automatically and may be overwritten." > $(installdir)WARNING.md
	cp -R $(staticdir)* $(installdir)

$(maincgi) : dir $(mainhs) $(datadir)
	printf "$(datadir)" > data/dir
	@~/.local/bin/stack build cs30:cs30.cgi --copy-bins --local-bin-path=$(installdir)
	@chmod 755 $(maincgi)

# Q: Why is there no make clean?
# I put in absolute paths to install scripts on the server.
# If I allowed 'clean' that removes all created files,
# there would be a chance of taking the server down inadvertently.
# Moreover, there is no safe way to remove the directory itself.
# Deleting $(installdir) should be done manually.
