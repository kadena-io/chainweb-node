
us1_REGION = us-east-1
us2_REGION = us-west-2

PROD_DNS_ZONE = chainweb.k8s.local

%.$(PROD_DNS_ZONE).yaml: cluster_template.yaml prod_vars.yaml
	test -n "$($*_REGION)" # $$($*_REGION)
	kops toolbox template --values prod_vars.yaml \
	--set clusterPrefix=$* \
	--set dnsZone=$(PROD_DNS_ZONE) \
	--set awsRegion=$($*_REGION) \
	--template cluster_template.yaml \
	--output $*.$(PROD_DNS_ZONE).yaml


%.$(PROD_DNS_ZONE): %.$(PROD_DNS_ZONE).yaml
	kops replace -f $*.$(PROD_DNS_ZONE).yaml --force
	kops create secret --name $*.$(PROD_DNS_ZONE) sshpublickey admin -i ~/.ssh/id_rsa.pub
	kops update cluster --name $*.$(PROD_DNS_ZONE) --yes
	kops get --name $*.$(PROD_DNS_ZONE) -o yaml > $*.$(PROD_DNS_ZONE)


%-prod-clean:
	kops delete cluster --name $*.$(PROD_DNS_ZONE) --yes
	rm -f $*.$(PROD_DNS_ZONE)
