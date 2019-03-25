#/bin/bash

kubectl --context=us2.chainweb.k8s.local create secret generic config --from-file=/Users/linda.ortega.cordoves/kube-bootstrap-node.config &&
kubectl --context=us2.chainweb.k8s.local apply -f ./us2_manual.yaml