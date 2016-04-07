# Testing Gadget MANUALLY (feelsbadman)

Since it is a little difficult to automate gadget tests, we have this list
of manual steps to test gadget.

## General steps:

- Create a dummy project \(OTP compliant\)
  * add some dummy modules
  * upload it to GitHub
- Start gadget \(./run\)
- Go to localhost:8080
- Click on button to `Sign in with github`
- Register the `webhooks` you want to use in your project
- Open a PR

**Note:** For gadget to run the webhooks you need to push your changes so the
PR gets synchronized \(Just Saying\)

## Rebar3

### compiler

Here is how `compiler` tool supports `rebar3`:

If there is a `rebar.config` file within the repository, gadget builds that
project using rebar, so if the repository has a `rebar` file, gadget uses it,
otherwise gadget check if there is a `rebar3` file and uses it, if not `rebar3`
file in there, gadget uses its own `rebar3` to build the project.

So, to test gadget-compiler rebar3 support, you need to follow these steps:

- add a `rebar.config` file in the root of the project
- Test according to the following scenarios

#### Scenarios

- Gadget uses project's `rebar`:
  * add `rebar` executable

- Gadget uses project's `rebar3`:
  * remove `rebar` and add `rebar3` executable

- Gadget uses `rebar3` from its dependencies:
  * remove `rebar` and `rebar3` files from repository

## erlang.mk

If not `rebar.config` file found, gadget uses `erlang.mk` build style \(make\).
So, if you want your project to be build using `erlang.mk`, then remove any
`rebar.config` file from your project.


### An image is better than 1k words

```
____________________
|    Gadget        |
--------------------
      | if
------------------------          else             --------------------
| project/rebar.config | ------------------------> | erlang.mk (make) |
------------------------                           --------------------
   if |
-------------------   else if    ------------------    else    ---------------------
| project/rebar   | -----------> | project/rebar3 | ---------> | deps/rebar/rebar3 |
-------------------              ------------------            ---------------------
```
