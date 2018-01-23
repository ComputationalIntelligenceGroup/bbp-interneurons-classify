clean: 
	find . ! -name 'Makefile'  -exec rm -r -f {} +

populate: 
	cd ~/code/bbp-interneurons-classify; git checkout standalone
	cp -r ~/code/bbp-interneurons-classify/* ~/code/bbp-interneurons-publish/
