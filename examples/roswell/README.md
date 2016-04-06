# Provisioning Roswell to remote host

This example shows a Lakefile that provisions Roswell to a remote host.

Execute `lake` command with environment variables `HOST` and `USER`, which specify hostname and optional user name for SSH connection.

    $ ls
    Lakefile README.md
    $ HOST=example.com USER=ubuntu lake
