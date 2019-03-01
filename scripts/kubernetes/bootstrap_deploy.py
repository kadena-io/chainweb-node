from os import path

import yaml
import base64

from kubernetes import client, config

NAMESPACE="default"
DEPLOYMENT_NAME = "chainweb"
DEPLOYMENT_IMAGE = "chainweb-bootstrap-test"
PORT_NUMBER = 1789
PER_REGION_NODES = 1

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


def create_pod_service(api_instance, pod_name):
    spec = client.V1ServiceSpec(
        type = "NodePort",
        selector = {"statefulset.kubernetes.io/pod-name": pod_name},
        ports = [client.V1ServicePort(
            port = 443,
            target_port = PORT_NUMBER,
        )]
    )
    service = client.V1Service(
        api_version = "v1",
        kind = "Service",
        metadata = client.V1ObjectMeta( name = pod_name ),
        spec = spec
    )
    api_instance.create_namespaced_service( namespace=NAMESPACE, body=service, pretty='true' )


def create_pod_template():
    # Configure volumes
    config_volume = client.V1Volume(
        name = "bootstrap-config-vol",
        secret = client.V1SecretVolumeSource( secret_name = "bootstrap-config" )
    )
    # Configure volume mounts
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
        isTTY = True
    container = client.V1Container(
        name  = "chainweb",
        image = DEPLOYMENT_IMAGE,
        image_pull_policy = "Never",    # When using local docker image
        tty = isTTY,
        ports = [ client.V1ContainerPort( 
            container_port = PORT_NUMBER ) ],
        volume_mounts = [config_mount, pv_mount]
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


def create_pod_template_without_pvc():
    # Configure volumes
    config_volume = client.V1Volume(
        name = "bootstrap-config-vol",
        secret = client.V1SecretVolumeSource( secret_name = "bootstrap-config" )
    )
    # Configure volume mounts
    config_mount = client.V1VolumeMount(
        mount_path = "/tmp",
        name = "bootstrap-config-vol"
    )
    # Configureate Pod template container
    isTTY = False
    if (DEPLOYMENT_IMAGE == "chainweb-base"):
        isTTY = True
    container = client.V1Container(
        name  = "chainweb",
        image = DEPLOYMENT_IMAGE,
        image_pull_policy = "Never",    # When using local docker image
        tty = isTTY,
        ports = [ client.V1ContainerPort( 
            container_port = PORT_NUMBER ) ],
        volume_mounts = [config_mount]
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


def create_volume_claim_template():
    return client.V1PersistentVolumeClaim(
        metadata = client.V1ObjectMeta (name = "data"),
        spec = client.V1PersistentVolumeClaimSpec (
            access_modes = [ "ReadWriteOnce" ],
            #storageClassName = "ebs", # Use default for now
            resources = client.V1ResourceRequirements(
                requests = {"storage": "1Gi"}
            )
        )
    )

def create_stateful_set_obj():
    spec = client.V1beta2StatefulSetSpec(
        pod_management_policy = "Parallel",
        replicas = PER_REGION_NODES,
        selector = client.V1LabelSelector(
            match_labels = {"app": "chainweb"}
        ),
        service_name = "chainweb-service",
        template = create_pod_template(),
        volume_claim_templates = [create_volume_claim_template()]
    )
    stateful_set = client.V1beta2StatefulSet(
        api_version = "apps/v1beta2",
        kind = "StatefulSet",
        metadata = client.V1ObjectMeta( name = DEPLOYMENT_NAME),
        spec = spec
    )
    return stateful_set



def create_deployment_obj():
    # Create the specification of deployment
    spec = client.ExtensionsV1beta1DeploymentSpec(
        replicas = PER_REGION_NODES,
        template = create_pod_template_without_pvc()
    )
    # Instantiate the deployment object
    deployment = client.ExtensionsV1beta1Deployment(
        api_version = "extensions/v1beta1",
        kind = "Deployment",
        metadata = client.V1ObjectMeta( name = DEPLOYMENT_NAME),
        spec = spec
    )

    return deployment



def create_stateful_set(api_instance,stateful_set):
    api_response = api_instance.create_namespaced_stateful_set(
        body=stateful_set,
        namespace=NAMESPACE,
        pretty='true')
    print("Statefule Set created. status='%s'" % str(api_response.status))


def create_deployment(api_instance, deployment):
    # Create deployement
    api_response = api_instance.create_namespaced_deployment(
        body=deployment,
        namespace=NAMESPACE,
        pretty='true')
    print(api_response)
    print("Deployment created. status='%s'" % str(api_response.status))


def update_deployment(api_instance, deployment):
    # Update container image
    deployment.spec.template.spec.containers[0].image = "nginx:1.9.1"
    # Update the deployment
    api_response = api_instance.patch_namespaced_deployment(
        name=DEPLOYMENT_NAME,
        namespace=NAMESPACE,
        body=deployment)
    print(api_response)
    print("Deployment updated. status='%s'" % str(api_response.status))


def delete_deployment(api_instance):
    # Delete deployment
    api_response = api_instance.delete_namespaced_deployment(
        name=DEPLOYMENT_NAME,
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(
            propagation_policy='Foreground',
            grace_period_seconds=5))
    print("Deployment deleted. status='%s'" % str(api_response.status))

def delete_secret(api_instance):
    api_instance.delete_namespaced_secret(
        name="bootstrap-config", 
        namespace=NAMESPACE,
        body=client.V1DeleteOptions()
    )


def delete_headless_service(api_instance):
    api_instance.delete_namespaced_service(
        name="chainweb-headless",
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(),
        pretty = "true"
    )

def delete_pod_service(api_instance, pod_name):
    api_instance.delete_namespaced_service(
        name=pod_name,
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(),
        pretty = "true"
    )

def delete_persistent_volume(api_instance, name):
    api_instance.delete_namespaced_persistent_volume_claim(
        name=name,
        namespace=NAMESPACE,
        pretty = "true",
        body = client.V1DeleteOptions()
    )

def delete_stateful_set(api_instance):
    api_response = api_instance.delete_namespaced_stateful_set(
        name=DEPLOYMENT_NAME,
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(
            propagation_policy='Foreground',
            grace_period_seconds=5),
        pretty = "true")
    print("Stateful Set deleted. status='%s'" % str(api_response.status))


def main():
    # Configs can be set in Configuration class directly or using helper
    # utility. If no argument provided, the config will be loaded from
    # NAMESPACE location.
    config.load_kube_config()
    extensions_v1beta1 = client.ExtensionsV1beta1Api()
    core_v1 = client.CoreV1Api()
    # Create a deployment object with client-python API. The deployment we
    # created is same as the `nginx-deployment.yaml` in the /examples folder.

    create_secret_from_file(core_v1, "scripts/test-bootstrap-node.config")
    create_headless_service(core_v1)
    create_pod_service(core_v1,"chainweb-0")
    stateful_set = create_stateful_set_obj()
    create_stateful_set(client.AppsV1beta2Api(),stateful_set)

    #delete_stateful_set(client.AppsV1beta2Api())
    #delete_secret(core_v1)
    #delete_headless_service(core_v1)
    #delete_pod_service(core_v1,"chainweb-0")
    #delete_persistent_volume(core_v1, "data-chainweb-0")


    #deployment = create_deployment_obj()
    #create_deployment(extensions_v1beta1,deployment)
    #delete_deployment(extensions_v1beta1)

if __name__ == '__main__':
    main()