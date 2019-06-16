R=R
RSCRIPT=Rscript
RCHECKER=language_server
NCPUS=4


.PHONY: tests

clean:
	rm -f  src/*.o src/*.so */*~ *~ src/*.rds manual.pdf
	rm -rf lib
	$(shell bash -c "shopt -s globstar && rm -f **/*.o **/*.so")

lib:
	mkdir -p $@

install: lib
	$(R) -e 'pkg=devtools::build(".", "lib");install.packages(pkg, "lib", INSTALL_opts = "--install-tests")'

coverage:
	$(R) -e 'covr::package_coverage()'

# tests require an installed package
tests: 
	R_LIBS=lib $(RSCRIPT) -e 'devtools::test()'

build:
	$(R) CMD build .

check: 
	$(R) -q -e 'devtools::check(check_dir = "checks")'


doc:
	$(R) CMD Rd2pdf -o manual.pdf .


################## docker checker ##################################
# directory in which the local dir is mounted inside the container
DIR=/root/rcpp_progress
DOCKER_RUN=docker run --rm -ti -v $(PWD):$(PWD) -w $(PWD) -u $$(id -u):$$(id -g) $(RCHECKER) 

docker/build:
	docker build -t $(RCHECKER) docker_checker

# check with r-base
docker/check: 
	#-docker rm  $(RCHECKER)
	$(DOCKER_RUN) make install check

docker/run: 
	#@-docker rm  $(RCHECKER)
	$(DOCKER_RUN) bash

docker/tests: 
	#@-docker rm  $(RCHECKER)
	$(DOCKER_RUN) make install tests


check_rhub_windows: 
	XDG_DATA_HOME=$(PWD) $(RSCRIPT) -e 'rhub::check_on_windows()'

win-builder-upload: build
	lftp  -u anonymous,karl.forner@gmail.com -e "set ftp:passive-mode true; cd R-release; mput *.tar.gz; cd ../R-devel;  mput *.tar.gz; bye" ftp://win-builder.r-project.org



