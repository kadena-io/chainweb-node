
# Table of Contents

1.  [Deploying Contracts](#org54b108c)
2.  [Sending Transactions from the Standard Contracts](#org3caea51)
3.  [Sending Simple Transactions](#org7f8694e)
4.  [Polling](#org509c5e7)
5.  [Listening](#org7c62e25)
6.  [Formats](#orgfee8702)
    1.  [List Format](#orge33b4d2)


<a id="org54b108c"></a>

# Deploying Contracts

- This can only be perfomed by providing the transaction generator
  script with the appropriate .yaml file. Look at either
  examples/local-templates/deploy-contracts-template.yaml or
  examples/testnet-templates/deploy-contracts-template.yaml for an
  example of how to configure this command.

<a id="org3caea51"></a>

# Sending Transactions from the Standard Contracts

- This can only be perfomed by providing the transaction generator
  script with the appropriate .yaml file. Look at either
  examples/local-templates/run-standard-contracts-template.yaml or
  examples/testnet-templates/run-standard-contracts-template.yaml for
  an example of how to configure this command.

<a id="org7f8694e"></a>

# Sending Simple Transactions

- This can only be perfomed by providing the transaction generator
  script with the appropriate .yaml file. Look at either
  examples/local-templates/run-simple-template.yaml or
  examples/testnet-templates/run-simple-template.yaml for an example
  of how to configure this command.

<a id="org509c5e7"></a>

# Polling

    <transaction-generator-binary> --script-command "poll <list of request keys> true|false"

-   Given a list of request keys, return the results of the
    transactions corresponding to each request key.

-   The format for lists is given below.

-   The bool flag is there to measure the amount of time it takes to
    poll the request key. This time is supposed to be logged. This may
    be removed.


<a id="org7c62e25"></a>

# Listening

    <transaction-generator-binary> --script-command "listen <request key> true|false"

-   Given a single request key, issue a listen api call with that
    request key. The result of the transaction associated with that
    request key should be returned if the result is returned before a
    HTTP timeout. The timeout is set by default to 4 minutes.

-   The bool flag is there to measure the amount of time it takes to
    listen upon the request key. This time is supposed to be logged.
    This may be removed.


<a id="orgfee8702"></a>

# Formats

<a id="orgc227a25"></a>

## List Format

    [item_1,item_2,...,item_n]

-   Lists must start with an open bracket ([) and end with a close bracket (]).
    &#x2013; There can be whitespace between the open bracket and the first item.
    &#x2013; There can be whitespace between the close bracket and the last item.
-   Items are comma-separated. There can be whitespace before and after the comma.
