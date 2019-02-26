from os import path

import yaml
import base64

from kubernetes import client, config

NAMESPACE="myproject"
DEPLOYMENT_NAME = "bootstrap-node-deployment"
DEPLOYMENT_IMAGE = "chainweb-base"
PORT_NUMBER = 1789
PER_REGION_NODES = 2

def create_secret_from_file(api_instance, filename):
    data = ""

    with open(filename, 'r') as f:
        data = f.read()
    #data = {"username": "bXl1c2VybmFtZQ==", "password": "bXlwYXNzd29yZA=="}

    secret = client.V1Secret(
        type = "Opaque",
        metadata = client.V1ObjectMeta( name="bootstrap-config" ),
        string_data = {"test-bootstrap-node.config" : data}
    )

    api_instance.create_namespaced_secret( namespace=NAMESPACE, body=secret )

def create_region_deployment_obj(region_str):
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
    container = client.V1Container(
        name  = "chainweb",
        image = DEPLOYMENT_IMAGE,
        image_pull_policy = "Never",    # When using local docker image
        tty = True,
        ports = [ client.V1ContainerPort(container_port = PORT_NUMBER) ],
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
    # Create the specification of deployment
    spec = client.ExtensionsV1beta1DeploymentSpec(
        replicas = PER_REGION_NODES,
        template = template
    )
    # Instantiate the deployment object
    deployment = client.ExtensionsV1beta1Deployment(
        api_version = "extensions/v1beta1",
        kind = "Deployment",
        metadata = client.V1ObjectMeta( name = DEPLOYMENT_NAME),
        spec = spec
    )

    return deployment


def create_deployment(api_instance, deployment):
    # Create deployement
    api_response = api_instance.create_namespaced_deployment(
        body=deployment,
        namespace=NAMESPACE)
    print("Deployment created. status='%s'" % str(api_response.status))


def update_deployment(api_instance, deployment):
    # Update container image
    deployment.spec.template.spec.containers[0].image = "nginx:1.9.1"
    # Update the deployment
    api_response = api_instance.patch_namespaced_deployment(
        name=DEPLOYMENT_NAME,
        namespace=NAMESPACE,
        body=deployment)
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
    
    deployment = create_region_deployment_obj("us-east")
    create_deployment(extensions_v1beta1,deployment)

    #delete_deployment(extensions_v1beta1)
    #delete_secret(core_v1)

if __name__ == '__main__':
    main()