
# For demo
SECTIONS = us1 us2

#SECTIONS = us1 us2 eu1

us1_REGION = us-east-1
us2_REGION = us-west-2
eu1_REGION = eu-central-1

# ------------------ VARIABLES: DON'T CHANGE --------------

KOPS_DNS_ZONE = chainweb.k8s.local
EXTERNAL_DNS_ZONE = chainweb.com
IMAGE = kadena/chainweb-bootstrap-node:testnet00-demo

.DEFAULT_GOAL := deploys


# ------------------ INFRASTRUCTURE ------------------

CREATE_CLUSTERS = $(addprefix create-cluster-, $(SECTIONS))
CHECK_CLUSTERS = $(addprefix check-cluster-, $(SECTIONS))
DELETE_CLUSTERS = $(addprefix delete-cluster-, $(SECTIONS))

$(CREATE_CLUSTERS): create-cluster-%: %.$(KOPS_DNS_ZONE)
$(CHECK_CLUSTERS): check-cluster-%: %-ready
$(DELETE_CLUSTERS): delete-cluster-%: %-shutdown


clusters: $(CREATE_CLUSTERS)
check-clusters: $(CHECK_CLUSTERS)
delete-clusters: $(DELETE_CLUSTERS)


%.$(KOPS_DNS_ZONE).yaml: cluster_template.yaml
	test -n "$($*_REGION)" # $$($*_REGION)
	kops toolbox template --template cluster_template.yaml \
	--set kopsStateStore=s3://$($*_REGION)-kops-state-store \
	--set masterSize=t2.medium \
	--set nodeSize=t2.small \
	--set nodesMinCount=2 \
	--set nodesMaxCount=2 \
	--set clusterPrefix=$* \
	--set dnsZone=$(KOPS_DNS_ZONE) \
	--set awsRegion=$($*_REGION) \
	--output $@


%.$(KOPS_DNS_ZONE): %.$(KOPS_DNS_ZONE).yaml
	aws s3 mb s3://$($*_REGION)-kops-state-store
	kops replace -f $@.yaml --force --state=s3://$($*_REGION)-kops-state-store
	kops create secret --name $@ sshpublickey admin -i ~/.ssh/id_rsa.pub \
		--state=s3://$($*_REGION)-kops-state-store
	kops update cluster --name $@ --yes --state=s3://$($*_REGION)-kops-state-store
	kops get --name $@ -o yaml --state=s3://$($*_REGION)-kops-state-store > $@ 


.PHONY: %-ready
%-ready:
	kops validate cluster --name $*.$(KOPS_DNS_ZONE) --state=s3://$($*_REGION)-kops-state-store
	kops rolling-update cluster --name $*.$(KOPS_DNS_ZONE) --state=s3://$($*_REGION)-kops-state-store


.PHONY: %-shutdown
%-shutdown:
	-kops delete cluster --name $*.$(KOPS_DNS_ZONE) --yes --state=s3://$($*_REGION)-kops-state-store
	-aws s3 rb s3://$($*_REGION)-kops-state-store --force
	$(RM) $*.$(KOPS_DNS_ZONE)



# ------------------ DEPLOYMENT ------------------

SEND_DEPLOYS = $(addprefix send-deploy-, $(SECTIONS))
CLEANUP_DEPLOYS = $(addprefix cleanup-deploy-, $(SECTIONS))

$(SEND_DEPLOYS): send-deploy-%: %-deploy
$(CLEANUP_DEPLOYS): cleanup-deploy-%: %-cleanup

deploys: check-clusters $(SEND_DEPLOYS)
cleanup-deploys: $(CLEANUP_DEPLOYS)


%.$(EXTERNAL_DNS_ZONE)-cert.pem: ~/letsencrypt/config/live/%.$(EXTERNAL_DNS_ZONE)/
	cp ~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/fullchain.pem  $@


%.$(EXTERNAL_DNS_ZONE)-privkey.pem: ~/letsencrypt/config/live/%.$(EXTERNAL_DNS_ZONE)/
	cp ~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/privkey.pem  $@

%_external_dns.yaml: external_dns_template.yaml
	kops toolbox template --template external_dns_template.yaml \
	--set subDomain=$* \
	--output $@

DEPLOY_DEPS := ../bootstrap_deploy.py  ./bootstrap-node.config

.PHONY: %-deploy
%-deploy: $(DEPLOY_DEPS)  %_external_dns.yaml  %.$(EXTERNAL_DNS_ZONE)-cert.pem   %.$(EXTERNAL_DNS_ZONE)-privkey.pem
	kubectl config use-context $*.$(KOPS_DNS_ZONE) && \
	kubectl apply -f $*_external_dns.yaml && \
	python ../bootstrap_deploy.py create \
	--subDomain=$* \
	--image=$(IMAGE) \
	--nodeConfig=./bootstrap-node.config \
	--certificate=$*.$(EXTERNAL_DNS_ZONE)-cert.pem \
	--privateKey=$*.$(EXTERNAL_DNS_ZONE)-privkey.pem



.PHONY: %-cleanup
%-cleanup: %-ready ../bootstrap_deploy.py
	kubectl config use-context $*.$(KOPS_DNS_ZONE) && \
	python ../bootstrap_deploy.py delete