
# Table of Contents

1.  [Deploying Contracts](#org54b108c)
2.  [Sending Transactions from the Standard Contracts](#org3caea51)
3.  [Sending Simple Transactions](#org7f8694e)
4.  [Polling](#org509c5e7)
5.  [Listening](#org7c62e25)
6.  [Formats](#orgfee8702)
    1.  [Timing Distribution](#orgc227a25)
    2.  [List Format](#orge33b4d2)


<a id="org54b108c"></a>

# Deploying Contracts

    <transaction-generator-binary> --script-command "deploy <list of filenames> true|false"

-   If the empty list is given then the coin, hello world and simple
    payments contract are loaded.

-   The bool flag is there to measure the amount of time it takes to
    deploy your contracts. This time is supposed to be logged. This is
    may be removed.


<a id="org3caea51"></a>

# Sending Transactions from the Standard Contracts

    <transaction-generator-binary> --script-command "run-standard <distribution> true|false"

-   This sends transactions specified in the coin, hello world and
    simple payments contract.

-   The format for distribution is given below. This specifies a
    probability distribution to vary the delays between successive
    transaction transmissions.

-   The bool flag is there to measure the amount of time it takes to
    send a particular transaction. This time is supposed to be logged.
    There is an asynchronous listen issued after the request key for
    each transaction is received from the server.


<a id="org7f8694e"></a>

# Sending Simple Transactions

    - <transaction-generator-binary> --script-command "run-simple <distribution> true|false"

-   This sends simple transactions like "(+ 2 2)" or "(\* 12 46)". The
    only operators currently included are multiplication, adddition and
    subtraction.

-   The format for distribution is given below. This specifies a
    probability distribution to vary the delays between successive
    transaction transmissions.

-   The bool flag is there to measure the amount of time it takes to
    send a particular transaction. This time is supposed to be logged.
    There is an asynchronous listen issued after the request key for
    each transaction is received from the server.


<a id="org509c5e7"></a>

# Polling

    <transaction-generator-binary> --script-command "poll <list of request keys> true|false"

-   Given a list of request keys, return the results of the
    transactions corresponding to each request key.

-   The format for lists is given below.

-   The bool flag is there to measure the amount of time it takes to
    poll the request key. This time is supposed to be logged. This is
    may be removed.


<a id="org7c62e25"></a>

# Listening

    <transaction-generator-binary> --script-command "listen <request key> true|false"

-   Given a single request key, issue a listen api call with that
    request key. The result of the transaction associated with that
    request key should be returned if the result is returned before a
    HTTP timeout.

-   The bool flag is there to measure the amount of time it takes to
    listen upon the request key. This time is supposed to be logged. This is
    may be removed.


<a id="orgfee8702"></a>

# Formats


<a id="orgc227a25"></a>

## Timing Distribution

    gaussian <mean:float> <variance:float>

OR

    uniform <lower-bound:float> <upper-bound:float>

-   Currently, only the Gaussian and Uniform distributions are supported.


<a id="orge33b4d2"></a>

## List Format

    [item_1,item_2,...,item_n]

-   Lists must start with an open bracket ([) and end with a close bracket (]).
    &#x2013; There is no space between the open bracket and the first item.
    &#x2013; There is no space between the close bracket and the first item.
-   Items are comma-separated. No spaces are allowed before or
    after the comma.
-   Each item must be satisfy this regular expression: [a-zA-Z\&#x00ad;0-9]+
    &#x2013; The ".pact" suffix is appended when the file is scheduled to be
    loaded.
