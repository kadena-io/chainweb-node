
# For demo
#SECTIONS = us1

#SECTIONS = us1 eu1 eu2

SECTIONS = us2

us1_REGION = us-east-1
us2_REGION = us-west-2
eu1_REGION = eu-central-1
eu2_REGION = eu-west-2

# ------------------ VARIABLES: DON'T CHANGE --------------

KOPS_DNS_ZONE = chainweb.k8s.local
EXTERNAL_DNS_ZONE = chainweb.com
IMAGE = kadena/chainweb-bootstrap-node:testnet00-party

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
	--set masterSize=t3.large \
	--set nodeSize=t3.xlarge \
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
	kops rolling-update cluster --name $*.$(KOPS_DNS_ZONE) --state=s3://$($*_REGION)-kops-state-store --yes


.PHONY: %-shutdown
%-shutdown:
	-kops delete cluster --name $*.$(KOPS_DNS_ZONE) --yes --state=s3://$($*_REGION)-kops-state-store
	-aws s3 rb s3://$($*_REGION)-kops-state-store --force
	$(RM) $*.$(KOPS_DNS_ZONE)



# ------------------ DEPLOYMENT ------------------

SEND_DEPLOYS = $(addprefix send-deploy-, $(SECTIONS))
CLEANUP_DEPLOYS = $(addprefix cleanup-deploy-, $(SECTIONS))
CHECK_DEPLOYS = $(addprefix check-deploy-, $(SECTIONS))

$(SEND_DEPLOYS): send-deploy-%: %-deploy
$(CLEANUP_DEPLOYS): cleanup-deploy-%: %-cleanup
$(CHECK_DEPLOYS): check-deploy-%: %-status

deploys: check-clusters $(SEND_DEPLOYS)
cleanup-deploys: $(CLEANUP_DEPLOYS)
check-deploys: $(CHECK_DEPLOYS)

.PHONY: %.$(EXTERNAL_DNS_ZONE)-configs
%.$(EXTERNAL_DNS_ZONE)-configs: ~/letsencrypt/config/live/%.$(EXTERNAL_DNS_ZONE)/
	python ../get_certificate_configs.py ~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/fullchain.pem \
	~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/privkey.pem
	kubectl --context=$*.$(KOPS_DNS_ZONE) create secret generic config --from-file=./bootstrap-node.config
	kubectl --context=$*.$(KOPS_DNS_ZONE) create secret generic cert --from-file=./fullchain.config
	kubectl --context=$*.$(KOPS_DNS_ZONE) create secret generic key --from-file=./privkey.config.config

%_external_dns.yaml: external_dns_template.yaml
	kops toolbox template --template external_dns_template.yaml \
	--set subDomain=$* \
	--output $@

DEPLOY_DEPS := ../bootstrap_deploy.py  ./bootstrap-node.config ../get_certificate_configs.py

.PHONY: %-deploy
%-deploy: $(DEPLOY_DEPS)  %_external_dns.yaml  %.$(EXTERNAL_DNS_ZONE)-configs
	kubectl --context=$*.$(KOPS_DNS_ZONE) apply -f $*_external_dns.yaml
	kubectl --context=$*.$(KOPS_DNS_ZONE) apply -f $*
	python ../bootstrap_deploy.py create \
	--context=$*.$(KOPS_DNS_ZONE) \
	--subDomain=$* \
	--image=$(IMAGE) \
	--nodeConfig=./bootstrap-node.config \
	--certificate=$*.$(EXTERNAL_DNS_ZONE)-cert.pem \
	--privateKey=$*.$(EXTERNAL_DNS_ZONE)-privkey.pem



.PHONY: %-cleanup
%-cleanup0: %-ready
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete secret config
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete secret key
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete secret cert
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete service $*-chainweb
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete deployment $*-chainweb

%-cleanup: %-ready
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete service/us2-chainweb
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete deployment/us2-chainweb

.PHONY: %-status
%-status:
	@echo "\n"
	@echo "------ STATUS FOR $* ------"
	@kubectl --context=$*.$(KOPS_DNS_ZONE) get all
	@kubectl --context=$*.$(KOPS_DNS_ZONE) get nodes