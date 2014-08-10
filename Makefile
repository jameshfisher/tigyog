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
	  --package     tigyog.deb                \
	  -t            deb                       \
	  -s            dir                       \
	  --depends     libssl-dev                \
	  --depends     libicu-dev                \
	  --maintainer  jameshfisher@gmail.com    \
	  --prefix      /opt/tigyog/              \
	  tigyog

clean:
	rm -rf dist/*
	rm -rf fpm/*
