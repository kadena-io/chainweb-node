#!/bin/bash

# Start minikube
eval `opam config env`
minikube delete &&
minikube start


# Save local docker images into minishift cluster
# Source: https://blogmilind.wordpress.com/2018/01/30/running-local-docker-images-in-kubernetes/
docker save chainweb-base > "chainweb-base.tar"
docker save chainweb-bootstrap-node > "chainweb-bootstrap-node.tar"

eval $(minikube docker-env)
docker load --input "chainweb-base.tar"
docker load --input "chainweb-bootstrap-node.tar"

rm "chainweb-base.tar"
rm "chainweb-bootstrap-node.tar"