-include Makefile.local

.DEFAULT_GOAL = build

build:
	dune build src/gwd.exe src/marshaler.exe

doc:
	dune build @doc-private

DOCDIR=_build/default/_doc/_html/

gh-pages: doc
	gitstatus=`git status --untracked-files=no --porcelain` \
	&& [ -z "$$gitstatus" ] \
	&& branch=`git symbolic-ref -q HEAD | cut -d "/" -f 3` \
	&& [ ! -z "$$branch" ] \
	&& commit=`git rev-parse HEAD` \
	&& tmp=`mktemp -d`/ \
	&& mv $(DOCDIR)types@gwd/Types/index.html $(DOCDIR)odoc.css $(DOCDIR)highlight.pack.js "$$tmp" \
	&& sed -i \
	-e 's|../../odoc.css|odoc.css|g' \
	-e 's|../../highlight.pack.js|highlight.pack.js|g' \
	-e 's|<nav><a href="../index.html">Up</a> – <a href="../index.html">types@gwd</a> &#x00BB; Types</nav><h1>Module <code>Types</code></h1>||g' \
	"$$tmp"/index.html \
	&& (! git show-ref --verify --quiet refs/heads/gh-pages \
	    || git branch -D -f gh-pages) \
	&& git checkout --orphan gh-pages \
	&& git rm -rf --ignore-unmatch . \
	&& git clean -df \
	&& mv "$$tmp"/* . \
	&& git add . \
	&& git commit -a -m "Build GitHub pages from commit $$commit" \
	&& git checkout "$$branch"

clean:
	dune clean

.PHONY: clean

launch:
	_build/default/src/gwd.exe \
	-p 2318 \
	-add_lexicon geneanet_utf8.txt \
	-bd /home/`whoami`/workspace/bases/ \
	-hd /home/`whoami`/workspace/geneanet.git/geneweb/gw_plus/gw \
	-td /home/`whoami`/workspace/geneanet.git/geneweb/gw_plus/gw/etc
