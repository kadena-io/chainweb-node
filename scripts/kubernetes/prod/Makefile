
SECTIONS = us1 us2 eu1

us1_REGION = us-east-1
us2_REGION = us-west-2
eu1_REGION = eu-central-1

# ------------------ VARIABLES: DON'T CHANGE --------------

KOPS_DNS_ZONE = chainweb.k8s.local
EXTERNAL_DNS_ZONE = chainweb.com

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


%.$(KOPS_DNS_ZONE).yaml: cluster_template.yaml prod_vars.yaml
	test -n "$($*_REGION)" # $$($*_REGION)
	kops toolbox template --values prod_vars.yaml \
	--set clusterPrefix=$* \
	--set dnsZone=$(KOPS_DNS_ZONE) \
	--set awsRegion=$($*_REGION) \
	--template cluster_template.yaml \
	--output $@


%.$(KOPS_DNS_ZONE): %.$(KOPS_DNS_ZONE).yaml
	kops replace -f $@.yaml --force
	kops create secret --name $@ sshpublickey admin -i ~/.ssh/id_rsa.pub
	kops update cluster --name $@ --yes
	kops get --name $@ -o yaml > $@


.PHONY: %-ready
%-ready:
	kops validate cluster --name $*.$(KOPS_DNS_ZONE)
	kops rolling-update cluster --name $*.$(KOPS_DNS_ZONE)


.PHONY: %-shutdown
%-shutdown:
	-kops delete cluster --name $*.$(KOPS_DNS_ZONE) --yes
	$(RM) $*.$(KOPS_DNS_ZONE)



# ------------------ DEPLOYMENT ------------------

SEND_DEPLOYS = $(addprefix send-deploy-, $(SECTIONS))
CLEANUP_DEPLOYS = $(addprefix cleanup-deploy-, $(SECTIONS))

$(SEND_DEPLOYS): send-deploy-%: %-deploy
$(CLEANUP_DEPLOYS): cleanup-deploy-%: %-cleanup

deploys: check-clusters $(SEND_DEPLOYS)
cleanup-deploys: $(CLEANUP_DEPLOYS)


%.$(EXTERNAL_DNS_ZONE)-cert.pem: ~/letsencrypt/config/live/%.$(EXTERNAL_DNS_ZONE)/
	cp ~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/cert.pem  $@


%.$(EXTERNAL_DNS_ZONE)-privkey.pem: ~/letsencrypt/config/live/%.$(EXTERNAL_DNS_ZONE)/
	cp ~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/privkey.pem  $@


DEPLOY_DEPS := external_dns.yaml  ../bootstrap_deploy.py

.PHONY: %-deploy
%-deploy: $(DEPLOY_DEPS)  %.$(EXTERNAL_DNS_ZONE)-cert.pem   %.$(EXTERNAL_DNS_ZONE)-privkey.pem
	kubectl config use-context $*.$(KOPS_DNS_ZONE) && \
	kubectl apply -f external_dns.yaml && \
	python ../bootstrap_deploy.py create


.PHONY: %-cleanup
%-cleanup: %-ready ../bootstrap_deploy.py
	kubectl config use-context $*.$(KOPS_DNS_ZONE) && \
	python ../bootstrap_deploy.py delete