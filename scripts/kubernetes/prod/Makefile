
us1_REGION = us-east-1
us2_REGION = us-west-2

DNS_ZONE = chainweb.k8s.local
EXTERNAL_DNS_ZONE = chainweb.com


%.$(DNS_ZONE).yaml: cluster_template.yaml prod_vars.yaml
	test -n "$($*_REGION)" # $$($*_REGION)
	kops toolbox template --values prod_vars.yaml \
	--set clusterPrefix=$* \
	--set dnsZone=$(DNS_ZONE) \
	--set awsRegion=$($*_REGION) \
	--template cluster_template.yaml \
	--output $*.$(DNS_ZONE).yaml


%.$(DNS_ZONE): %.$(DNS_ZONE).yaml
	kops replace -f $*.$(DNS_ZONE).yaml --force
	kops create secret --name $*.$(DNS_ZONE) sshpublickey admin -i ~/.ssh/id_rsa.pub
	kops update cluster --name $*.$(DNS_ZONE) --yes
	kops get --name $*.$(DNS_ZONE) -o yaml > $*.$(DNS_ZONE)


%.$(EXTERNAL_DNS_ZONE)-cert.pem: ~/letsencrypt/config/live/%.$(EXTERNAL_DNS_ZONE)/
	cp ~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/cert.pem  ./$*.$(EXTERNAL_DNS_ZONE)-cert.pem

%.$(EXTERNAL_DNS_ZONE)-privkey.pem: ~/letsencrypt/config/live/%.$(EXTERNAL_DNS_ZONE)/
	cp ~/letsencrypt/config/live/$*.$(EXTERNAL_DNS_ZONE)/privkey.pem  ./$*.$(EXTERNAL_DNS_ZONE)-privkey.pem


.PHONY: %-ready
%-ready:
	kops validate cluster --name $*.$(DNS_ZONE)
	kops rolling-update cluster --name $*.$(DNS_ZONE)


.PHONY: create-%-deploy
create-%-deploy: %-ready  external_dns.yaml  ../bootstrap_deploy.py  %.$(EXTERNAL_DNS_ZONE)-cert.pem  %.$(EXTERNAL_DNS_ZONE)-privkey.pem
	kubectl config use-context $*.$(DNS_ZONE)
	kubectl apply -f external_dns.yaml
	python ../bootstrap_deploy.py create

.PHONY: delete-%-deploy
delete-%-deploy: %-ready ../bootstrap_deploy.py
	kubectl config use-context $*.$(DNS_ZONE)
	python ../bootstrap_deploy.py delete



.PHONY: %-clean
%-clean:
	-kops delete cluster --name $*.$(DNS_ZONE) --yes
	$(RM) $*.$(PROD_DNS_ZONE)
