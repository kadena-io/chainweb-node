
# For demo
SECTIONS = us1 us2 eu1 as1

#SECTIONS = us1 us2 eu1

us1_REGION = us-east-1
us2_REGION = us-west-2
eu1_REGION = eu-central-1
as1_REGION = ap-southeast-2

# ------------------ VARIABLES: DON'T CHANGE --------------

KOPS_DNS_ZONE = miners.chainweb.k8s.local
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
	--set kopsStateStore=s3://$($*_REGION)-miner-kops-state-store \
	--set masterSize=t3.medium \
	--set nodeSize=t3.xlarge \
	--set nodesMinCount=2 \
	--set nodesMaxCount=2 \
	--set clusterPrefix=$* \
	--set dnsZone=$(KOPS_DNS_ZONE) \
	--set awsRegion=$($*_REGION) \
	--output $@


%.$(KOPS_DNS_ZONE): %.$(KOPS_DNS_ZONE).yaml
	aws s3 mb s3://$($*_REGION)-miner-kops-state-store
	kops replace -f $@.yaml --force --state=s3://$($*_REGION)-miner-kops-state-store
	kops create secret --name $@ sshpublickey admin -i ~/.ssh/id_rsa.pub \
		--state=s3://$($*_REGION)-miner-kops-state-store
	kops update cluster --name $@ --yes --state=s3://$($*_REGION)-miner-kops-state-store
	kops get --name $@ -o yaml --state=s3://$($*_REGION)-miner-kops-state-store > $@ 


.PHONY: %-ready
%-ready:
	kops validate cluster --name $*.$(KOPS_DNS_ZONE) --state=s3://$($*_REGION)-miner-kops-state-store
	kops rolling-update cluster --name $*.$(KOPS_DNS_ZONE) --state=s3://$($*_REGION)-miner-kops-state-store


.PHONY: %-shutdown
%-shutdown:
	-kops delete cluster --name $*.$(KOPS_DNS_ZONE) --yes --state=s3://$($*_REGION)-miner-kops-state-store
	-aws s3 rb s3://$($*_REGION)-miner-kops-state-store --force
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


%_miner1.yaml: miner_template.yaml
	kops toolbox template --template miner_template.yaml \
	--set image=$(IMAGE) \
	--set name=$*-miner1 \
	--output $@

%_miner2.yaml: miner_template.yaml
	kops toolbox template --template miner_template.yaml \
	--set image=$(IMAGE) \
	--set name=$*-miner2 \
	--output $@


.PHONY: %-deploy
%-deploy: %_miner1.yaml %_miner2.yaml
	kubectl --context=$*.$(KOPS_DNS_ZONE) apply -f $*_miner1.yaml
	kubectl --context=$*.$(KOPS_DNS_ZONE) apply -f $*_miner2.yaml


.PHONY: %-cleanup
%-cleanup: %-ready
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete pod/$*-miner1 service/$*-miner1
	@kubectl --context=$*.$(KOPS_DNS_ZONE) delete pod/$*-miner2 service/$*-miner2

.PHONY: %-status
%-status:
	@echo "\n"
	@echo "------ STATUS FOR $* ------"
	@kubectl --context=$*.$(KOPS_DNS_ZONE) get all
	@kubectl --context=$*.$(KOPS_DNS_ZONE) get nodes