from os import path

import sys
import yaml


def create_certificate_configs(certPath, keyPath):
    cert = ""
    with open(certPath, 'r') as f:
        cert = f.read()

    certYaml = dict(
        chainweb=dict(p2p=dict(peer=dict(certificateChain=cert))))

    with open('fullchain.config', 'w') as outfile:
        yaml.dump(certYaml, outfile, default_flow_style=False, default_style="|")

    key = ""
    with open(keyPath, 'r') as f:
        key = f.read()

    keyYaml = dict(chainweb=dict(p2p=dict(peer=dict(key=key))))

    with open('privkey.config', 'w') as outfile:
        yaml.dump(
            keyYaml, outfile, default_flow_style=False, default_style="|")



def main():
    print(sys.argv[1])
    #create_certificate_configs(sys.argv[1], sys.argv[2])


if __name__ == '__main__':
    main()