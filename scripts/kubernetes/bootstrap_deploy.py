from os import path

import sys
import yaml
import base64
from pick import pick

from kubernetes import client, config

NAMESPACE="default"
DEPLOYMENT_NAME = "chainweb"
DEPLOYMENT_IMAGE = "kadena/chainweb-bootstrap-node:v0"
DNS_NAME = ".chainweb.com."
PORT_NUMBER = 1789
PER_REGION_NODES = 1


# --- SECRETS
#     Adds bootstrap config file as a secret volume.

def create_secret_from_file(api_instance, filename):
    data = ""

    with open(filename, 'r') as f:
        data = f.read()

    secret = client.V1Secret(
        type = "Opaque",
        metadata = client.V1ObjectMeta( name="bootstrap-config" ),
        string_data = {"test-bootstrap-node.config" : data}
    )

    api_instance.create_namespaced_secret( namespace=NAMESPACE, body=secret, pretty='true' )


def delete_secret(api_instance):
    api_response = api_instance.delete_namespaced_secret(
        name="bootstrap-config",
        namespace=NAMESPACE,
        body=client.V1DeleteOptions()
    )
    print("Secret deleted. status='%s'" % str(api_response.status))



# --- HEADLESS SERVICE
#     Needed for working with StatefulSets

def create_headless_service(api_instance):
    spec = client.V1ServiceSpec(
        cluster_ip = "None",
        selector = {"app": "chainweb"},
        ports = [client.V1ServicePort(
            port = PORT_NUMBER,
            target_port = PORT_NUMBER
        )]
    )
    service = client.V1Service(
        api_version = "v1",
        kind = "Service",
        metadata = client.V1ObjectMeta( name = "chainweb-headless" ),
        spec = spec
    )
    api_instance.create_namespaced_service( namespace=NAMESPACE, body=service, pretty='true' )


def delete_headless_service(api_instance):
    api_response = api_instance.delete_namespaced_service(
        name="chainweb-headless",
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(),
        pretty = "true"
    )
    print("Headless Service deleted. status='%s'" % str(api_response.status))



# --- LOADBALANCER SERVICE
#     Manually creates a LoadBalancer service for each pod in the StatefulSet.
#     One of the ways to expose StatefulSet pods publically.
#     Makes the pod's application available at sub_domain.dns_name (i.e. us1.chainweb.com)

def create_pod_service(api_instance, sub_domain, pod_name):
    spec = client.V1ServiceSpec(
        type = "LoadBalancer",
        external_traffic_policy = "Local",
        selector = {"statefulset.kubernetes.io/pod-name": pod_name},
        ports = [client.V1ServicePort(
            port = 443,
            target_port = PORT_NUMBER,
        )]
    )
    service = client.V1Service(
        api_version = "v1",
        kind = "Service",
        metadata = client.V1ObjectMeta(
            name = pod_name,
            annotations = {
                "external-dns.alpha.kubernetes.io/hostname" : (sub_domain + DNS_NAME)
            }),
        spec = spec
    )
    api_instance.create_namespaced_service( namespace=NAMESPACE, body=service, pretty='true' )


def delete_pod_service(api_instance, pod_name):
    api_response = api_instance.delete_namespaced_service(
        name=pod_name,
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(),
        pretty = "true"
    )
    print("Pod Service deleted. status='%s'" % str(api_response.status))



# --- PERSISTENT VOLUME
#     Manually creates persistent volume for each pod in the StatefulSet.
#     Persistent volume claim required by StatefulSet.
#     Not deleted even after pods and StatefulSets shutdown.
#     Thus, need to be manually deleted.

def create_volume_claim_template():
    return client.V1PersistentVolumeClaim(
        metadata = client.V1ObjectMeta (name = "data"),
        spec = client.V1PersistentVolumeClaimSpec (
            access_modes = [ "ReadWriteOnce" ],
            resources = client.V1ResourceRequirements(
                requests = {"storage": "1Gi"}
            )
        )
    )


def delete_persistent_volume(api_instance, name):
    api_response = api_instance.delete_namespaced_persistent_volume_claim(
        name=name,
        namespace=NAMESPACE,
        pretty = "true",
        body = client.V1DeleteOptions()
    )
    print("Persistent volume deleted. status='%s'" % str(api_response.status))



# --- STATEFULSET
#     Creates StatefulSet for spinning up bootstrap node(s).

def create_pod_template_with_pvc():
    # Mount bootstrap config secret volume
    config_mount = client.V1VolumeMount(
        mount_path = "/tmp",
        name = "bootstrap-config-vol"
    )
    # PersistenVolume mount
    pv_mount = client.V1VolumeMount(
        mount_path = "/home", # better name?
        name = "data"
    )
    # Configureate Pod template container
    isTTY = False
    if (DEPLOYMENT_IMAGE == "chainweb-base"):
        isTTY = True        # otherwise container will always "finish" and restart.
    container = client.V1Container(
        name  = "chainweb",
        image = DEPLOYMENT_IMAGE,
        tty = isTTY,
        ports = [ client.V1ContainerPort( 
            container_port = PORT_NUMBER ) ],
        volume_mounts = [config_mount, pv_mount]
    )

    # Configure volume(s)
    config_volume = client.V1Volume(
        name = "bootstrap-config-vol",
        secret = client.V1SecretVolumeSource( secret_name = "bootstrap-config" )
    )

    # Create and configurate a spec section
    template = client.V1PodTemplateSpec(
        metadata = client.V1ObjectMeta( labels = {"app": "chainweb"} ),
        spec = client.V1PodSpec(
            containers = [container],
            volumes  = [config_volume] 
        )
    )
    return template


def create_stateful_set_obj():
    spec = client.V1beta2StatefulSetSpec(
        pod_management_policy = "Parallel",
        replicas = PER_REGION_NODES,
        selector = client.V1LabelSelector(
            match_labels = {"app": "chainweb"}
        ),
        service_name = "chainweb-service",
        template = create_pod_template_with_pvc(),
        volume_claim_templates = [create_volume_claim_template()]
    )
    stateful_set = client.V1beta2StatefulSet(
        api_version = "apps/v1beta2",
        kind = "StatefulSet",
        metadata = client.V1ObjectMeta( name = DEPLOYMENT_NAME),
        spec = spec
    )
    return stateful_set


def create_stateful_set(api_instance):
    stateful_set = create_stateful_set_obj()
    api_response = api_instance.create_namespaced_stateful_set(
        body=stateful_set,
        namespace=NAMESPACE,
        pretty='true')
    print("Statefule Set created. status='%s'" % str(api_response.status))


def delete_stateful_set(api_instance):
    api_response = api_instance.delete_namespaced_stateful_set(
        name=DEPLOYMENT_NAME,
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(
            propagation_policy='Foreground',
            grace_period_seconds=5),
        pretty = "true")
    print("Stateful Set deleted. status='%s'" % str(api_response.status))



# --- CREATES or DELETES Chainweb's bootstrap kubernetes infrastructure
#     in a particular context.
#     Thus allows for creating/deleting resources in different kubernetes clusters.

def main():
    if (len(sys.argv) == 2) and (sys.argv[1] in ["create","delete"]):
        isCreate = True if (sys.argv[1] == "create") else False
        run_valid_action(isCreate)
    else:
        print("Expected ONE of the following:" \
              "\n\t`python <app>.py create`" \
              "\n\t`python <app>.py delete\n" \
              "But received: " + str(sys.argv))
        return


def run_valid_action(isCreate):
    # Ask user which context (i.e. cluster) they want to work with
    contexts, active_context = config.list_kube_config_contexts()
    if not contexts:
        print("Cannot find any context in kube-config file.")
        return
    contexts = [context['name'] for context in contexts]
    active_index = contexts.index(active_context['name'])
    option, _ = pick(contexts, title="Pick the context to load",
                     default_index=active_index)
    config.load_kube_config(context=option)

    if isCreate:
        create_resources()
    else:
        delete_resources()


def create_resources():
    apps_v1beta2 = client.AppsV1beta2Api()
    core_v1 = client.CoreV1Api()

    create_secret_from_file(core_v1, "scripts/kubernetes/kube-bootstrap-node.config")
    create_headless_service(core_v1)
    create_pod_service(core_v1,"us1","chainweb-0")
    create_stateful_set(apps_v1beta2)


def delete_resources():
    apps_v1beta2 = client.AppsV1beta2Api()
    core_v1 = client.CoreV1Api()

    try_delete(delete_stateful_set, apps_v1beta2)
    try_delete(delete_secret, core_v1)
    try_delete(delete_headless_service,core_v1)
    try_delete(delete_pod_service,core_v1,"chainweb-0")
    try_delete(delete_persistent_volume,core_v1, "data-chainweb-0")


def try_delete(delete_func, api_instance, arg=None):
    try:
        if arg:
            delete_func(api_instance,arg)
        else:
            delete_func(api_instance)
    except Exception as e:
        print("Something went wrong: " + str(e))
        pass

if __name__ == '__main__':
    main()