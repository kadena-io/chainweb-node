#!/bin/bash

# Start minikube
eval `opam config env`
minikube delete &&
minikube start --vm-driver=hyperkit


# Save local docker images into minishift cluster
# Source: https://blogmilind.wordpress.com/2018/01/30/running-local-docker-images-in-kubernetes/
docker save chainweb-base > chainweb-base-image.tar
docker save chainweb-bootstrap-test > chainweb-bootstrap-test.tar

eval $(minikube docker-env)
docker load --input chainweb-base-image.tar
docker load --input chainweb-bootstrap-test.tar

rm chainweb-base-image.tar
rm chainweb-bootstrap-test.tar