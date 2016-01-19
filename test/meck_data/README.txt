In order to avoid using GitHub API within tests, the tests use payloads
(located in this folder) with meck.

The payloads filename represents the module name and the function (separated
by two underscore) the data is used for, e.g: module__function_name.txt

Payloads by TestCase:

+ gadget_SUITE:valid_organization_repositories_test/1
    - egithub__orgs.txt -> content is send when egithub:orgs/1 is called.
    - egithub__all_org_repos.txt -> content is send when egithub:all_org_repos/3 is called.
    - egithub__hooks.txt -> content is send when egithub:hooks/2 is called.
