## For developers



### tests and check

If you have all the dependencies (and suggests) installed:

type:
 - `make tests`: to run the tests
 - `make check`: to check the package

### docker-checker

A Dockerfile (<docker_checker/Dockerfile>) is provided to help building the
dev environment in which to develop and test the R package.

type:

 - `make docker/build`: to build the docker
 - `make docker/run`: to run a shell in the docker with the current dir mounted
 	inside
 - `make docker/check`: to check the package inside the docker
 - `make docker/tests`: to run test tests of the package inside the docker

### test on windows using rhub

not working yet...

```
make docker/run
make check_rhub_windows
```
