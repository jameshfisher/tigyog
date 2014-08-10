VERSION=$(shell runhaskell version.hs)

dist/build/tigyog/tigyog:
	cabal clean
	cabal configure
	cabal build

# This corresponds to /opt/tigyog
fpm:
	mkdir -p $@

fpm/tigyog: fpm dist/build/tigyog/tigyog
	cp dist/build/tigyog/tigyog $@

fpm/tigyog.deb: fpm/tigyog
	cd fpm && \
	fpm                                       \
	  --name        tigyog                    \
	  --description 'Git project management'  \
	  --version     ${VERSION}                \
	  --package     tigyog.deb                \
	  -t            deb                       \
	  -s            dir                       \
	  --depends     libssl-dev                \
	  --depends     libicu-dev                \
	  --maintainer  jameshfisher@gmail.com    \
	  --vendor      jameshfisher@gmail.com    \
	  --url         http://tigyog.org/        \
	  --license     GPL3                      \
	  --prefix      /opt/tigyog/              \
	  tigyog

clean:
	rm -rf dist/*
	rm -rf fpm/*
