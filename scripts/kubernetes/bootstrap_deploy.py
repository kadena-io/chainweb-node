from os import path

import argparse
import base64
import sys
import yaml

from kubernetes import client, config
from kubernetes.client import configuration


NAMESPACE = "default"
DEPLOYMENT_NAME = "chainweb"
DNS_NAME = ".chainweb.com"
PORT_NUMBER = 1789
PER_CLUSTER_NODES = 1

# --- SECRETS
#     Adds bootstrap config file as a secret volume.


def create_secret_from_file(api_instance, configPath, certPath, keyPath):
    config = ""
    with open(configPath, 'r') as f:
        config = f.read()

    data = {
        "node.config": config,
    }

    # if certificate file provided
    if certPath:
        cert = ""
        with open(certPath, 'r') as f:
            cert = f.read()

        certYaml = dict(
            chainweb=dict(p2p=dict(peer=dict(certificateChain=cert))))
        data["cert.config"] = str(
            yaml.dump(certYaml, default_flow_style=False, default_style="|"))

    # if certificate private key file provided
    if keyPath:
        key = ""
        with open(keyPath, 'r') as f:
            key = f.read()

        keyYaml = dict(chainweb=dict(p2p=dict(peer=dict(key=key))))
        data["privkey.config"] = str(
            yaml.dump(keyYaml, default_flow_style=False, default_style="|"))

    secret = client.V1Secret(
        type="Opaque",
        metadata=client.V1ObjectMeta(name="bootstrap-config"),
        string_data=data)

    api_instance.create_namespaced_secret(
        namespace=NAMESPACE, body=secret, pretty='true')


def delete_secret(api_instance):
    api_response = api_instance.delete_namespaced_secret(
        name="bootstrap-config",
        namespace=NAMESPACE,
        body=client.V1DeleteOptions())
    print("Secret deleted. status='%s'" % str(api_response.status))


# --- HEADLESS SERVICE
#     Needed for working with StatefulSets


def create_headless_service(api_instance):
    spec = client.V1ServiceSpec(
        cluster_ip="None",
        selector={"app": "chainweb"},
        ports=[
            client.V1ServicePort(port=PORT_NUMBER, target_port=PORT_NUMBER)
        ])
    service = client.V1Service(
        api_version="v1",
        kind="Service",
        metadata=client.V1ObjectMeta(name="chainweb-headless"),
        spec=spec)
    api_instance.create_namespaced_service(
        namespace=NAMESPACE, body=service, pretty='true')


def delete_headless_service(api_instance):
    api_response = api_instance.delete_namespaced_service(
        name="chainweb-headless",
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(),
        pretty="true")
    print("Headless Service deleted. status='%s'" % str(api_response.status))


# --- LOADBALANCER SERVICE
#     Manually creates a LoadBalancer service for each pod in the StatefulSet.
#     One of the ways to expose StatefulSet pods publically.
#     Makes the pod's application available at sub_domain.dns_name (i.e. us1.chainweb.com)


def create_pod_service(api_instance, sub_domain, pod_name):
    spec = client.V1ServiceSpec(
        type="LoadBalancer",
        external_traffic_policy="Local",
        selector={"statefulset.kubernetes.io/pod-name": pod_name},
        ports=[client.V1ServicePort(
            port=443,
            target_port=PORT_NUMBER,
        )])
    service = client.V1Service(
        api_version="v1",
        kind="Service",
        metadata=client.V1ObjectMeta(
            name=pod_name,
            annotations={
                "external-dns.alpha.kubernetes.io/hostname":
                (sub_domain + DNS_NAME)
            }),
        spec=spec)
    api_instance.create_namespaced_service(
        namespace=NAMESPACE, body=service, pretty='true')


def delete_pod_service(api_instance, pod_name):
    api_response = api_instance.delete_namespaced_service(
        name=pod_name,
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(),
        pretty="true")
    print("Pod Service deleted. status='%s'" % str(api_response.status))


# --- PERSISTENT VOLUME
#     Manually creates persistent volume for each pod in the StatefulSet.
#     Persistent volume claim required by StatefulSet.
#     Not deleted even after pods and StatefulSets shutdown.
#     Thus, need to be manually deleted.


def create_volume_claim_template():
    return client.V1PersistentVolumeClaim(
        metadata=client.V1ObjectMeta(name="data"),
        spec=client.V1PersistentVolumeClaimSpec(
            access_modes=["ReadWriteOnce"],
            resources=client.V1ResourceRequirements(
                requests={"storage": "1Gi"})))


def delete_persistent_volume(api_instance, name):
    api_response = api_instance.delete_namespaced_persistent_volume_claim(
        name=name,
        namespace=NAMESPACE,
        pretty="true",
        body=client.V1DeleteOptions())
    print("Persistent volume deleted. status='%s'" % str(api_response.status))


# --- STATEFULSET
#     Creates StatefulSet for spinning up bootstrap node(s).


def create_pod_template_with_pvc(args):
    # Mount bootstrap config secret volume
    config_mount = client.V1VolumeMount(
        mount_path="/tmp", name="bootstrap-config-vol")
    # PersistenVolume mount
    pv_mount = client.V1VolumeMount(
        mount_path="/home",  # better name?
        name="data")

    # Configureate Pod template container
    isTTY = args.image == "chainweb-base"  # otherwise container will always "finish" and restart.
    pull_policy = "Never" if args.local else "Always"
    command = [
        "/bin/chainweb-node", "--config-file=/tmp/node.config",
        "--telemetry-log-handle=es:search-tnlogs-eb57oi7cimmanlguprazntbqva.us-east-1.es.amazonaws.com:80",
        "--log-handle=es:search-tnlogs-eb57oi7cimmanlguprazntbqva.us-east-1.es.amazonaws.com:80",
        "--disable-transaction-index"
    ]

    # if certificate file was present
    if args.certificate:
        command.append("--config-file=/tmp/cert.config")
    # if certificate private key was present
    if args.privateKey:
        command.append("--config-file=/tmp/privkey.config")

    container = client.V1Container(
        name="chainweb",
        image=args.image,
        command=command,
        resources=client.V1ResourceRequirements(requests={
            "cpu": "400m",
            "memory": "14Gi"
        }),
        image_pull_policy=pull_policy,
        tty=isTTY,
        ports=[client.V1ContainerPort(container_port=PORT_NUMBER)],
        volume_mounts=[config_mount, pv_mount])

    # Configure volume(s)
    config_volume = client.V1Volume(
        name="bootstrap-config-vol",
        secret=client.V1SecretVolumeSource(secret_name="bootstrap-config"))

    # Create and configurate a spec section
    template = client.V1PodTemplateSpec(
        metadata=client.V1ObjectMeta(labels={"app": "chainweb"}),
        spec=client.V1PodSpec(containers=[container], volumes=[config_volume]))
    return template


def create_stateful_set_obj(args):
    spec = client.V1beta2StatefulSetSpec(
        pod_management_policy="Parallel",
        replicas=PER_CLUSTER_NODES,
        selector=client.V1LabelSelector(match_labels={"app": "chainweb"}),
        service_name="chainweb-service",
        template=create_pod_template_with_pvc(args),
        volume_claim_templates=[create_volume_claim_template()])
    stateful_set = client.V1beta2StatefulSet(
        api_version="apps/v1beta2",
        kind="StatefulSet",
        metadata=client.V1ObjectMeta(name=DEPLOYMENT_NAME),
        spec=spec)
    return stateful_set


def create_stateful_set(api_instance, args):
    stateful_set = create_stateful_set_obj(args)
    api_response = api_instance.create_namespaced_stateful_set(
        body=stateful_set, namespace=NAMESPACE, pretty='true')
    print("Statefule Set created. status='%s'" % str(api_response.status))


def delete_stateful_set(api_instance):
    api_response = api_instance.delete_namespaced_stateful_set(
        name=DEPLOYMENT_NAME,
        namespace=NAMESPACE,
        body=client.V1DeleteOptions(
            propagation_policy='Foreground', grace_period_seconds=5),
        pretty="true")
    print("Stateful Set deleted. status='%s'" % str(api_response.status))


# --- CREATES or DELETES Chainweb's bootstrap kubernetes infrastructure
#     in a particular context.
#     Thus allows for creating/deleting resources in different kubernetes clusters.


def arg_parsing():
    parser = argparse.ArgumentParser(
        description="Control Chainweb node deployment to Kubernetes")
    subparsers = parser.add_subparsers()

    # --- `create` flags --- #
    create_p = subparsers.add_parser(
        "create", help="Create a Chainweb node cluster")

    create_p.add_argument(
        "--image",
        default="kadena/chainweb-bootstrap-node:latest",
        help="A docker image to run")

    create_p.add_argument(
        "--subDomain",
        default="testing",
        help="Sub-domain name for application to run externally")

    create_p.add_argument(
        "--local",
        action="store_true",
        help="Assume the docker image can be found locally")

    create_p.add_argument(
        "--logToElasticSearch",
        action="store_true",
        help="Sends logs to elastic search logger")

    create_p.add_argument(
        "--context",
        help="kubectl config context to use")

    create_p.add_argument("--certificate", help="Filename of CA certificate.")

    create_p.add_argument(
        "--privateKey", help="Filename of CA certificate private key.")

    # TODO make a mandatory argument for `create`
    create_p.add_argument(
        "--nodeConfig", help="Filename of chainweb node config.")

    create_p.set_defaults(func=create_resources)

    # --- `delete` flags --- #
    delete_p = subparsers.add_parser(
        "delete", help="Tear down a Chainweb node cluster")

    delete_p.add_argument("--context", help="kubectl config context to use")

    delete_p.set_defaults(func=delete_resources)

    # --- `update` flags --- #
    update_p = subparsers.add_parser(
        "update", help="Update Chainweb node deployment component(s)")

    update_p.add_argument(
        "--image",
        default="kadena/chainweb-bootstrap-node:latest",
        help="A docker image to update deployment with")

    update_p.add_argument("--context", help="kubectl config context to use")

    update_p.set_defaults(func=update_resources)

    return parser.parse_args()


def main():
    args = arg_parsing()

    contexts, active_context = config.list_kube_config_contexts()

    if not contexts:
        print("Cannot find any context in kube-config file.")
        return

    contexts = [context['name'] for context in contexts]
    active_index = contexts.index(active_context['name'])

    if args.context:
        new_index = contexts.index(args.context)
        option = contexts[new_index]
        config.load_kube_config(context=option)

        print("Active host is %s" % option)

        print("Running with: ", args)
        args.func(args, option)

        print("Done")
    else:
        option = contexts[active_index]
        config.load_kube_config(context=option)

        print("Active host is %s" % option)

        print("Running with: ", args)
        args.func(args, option)

        print("Done")





def create_resources(args, context):
    print("Creating cluster...")

    apps_v1beta2 = client.AppsV1beta2Api(
        api_client=config.new_client_from_config(context=context))
    core_v1 = client.CoreV1Api(
        api_client=config.new_client_from_config(context=context))

    create_secret_from_file(core_v1, args.nodeConfig, args.certificate,
                            args.privateKey)
    create_headless_service(core_v1)
    create_pod_service(core_v1, args.subDomain, "chainweb-0")
    create_stateful_set(apps_v1beta2, args)


def update_resources(args, context):
    print("Updating cluster...")

    apps_v1beta2 = client.AppsV1beta2Api(
        api_client=config.new_client_from_config(context=context))

    stateful_set_patch = {
        "spec": {
            "template": {
                "spec": {
                    "containers": [{
                        "name": "chainweb",
                        "image": args.image
                    }]
                }
            }
        }
    }

    api_response = apps_v1beta2.patch_namespaced_stateful_set(
        DEPLOYMENT_NAME, NAMESPACE, stateful_set_patch, pretty=True)
    print("Container image updated. status='%s'" % str(api_response.status))


def delete_resources(args, context):
    print("Deleting cluster...")

    apps_v1beta2 = client.AppsV1beta2Api(
        api_client=config.new_client_from_config(context=context))
    core_v1 = client.CoreV1Api(
        api_client=config.new_client_from_config(context=context))

    try_delete(delete_stateful_set, apps_v1beta2)
    try_delete(delete_secret, core_v1)
    try_delete(delete_headless_service, core_v1)
    try_delete(delete_pod_service, core_v1, "chainweb-0")
    try_delete(delete_persistent_volume, core_v1, "data-chainweb-0")


def try_delete(delete_func, api_instance, arg=None):
    try:
        if arg:
            delete_func(api_instance, arg)
        else:
            delete_func(api_instance)
    except Exception as e:
        print("Something went wrong: " + str(e))
        pass


if __name__ == '__main__':
    main()