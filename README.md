# ics-updater

This program downloads an ICS calendar from a URL and removes events based on a set of rules, either matching on UIDs, or searching for terms in the summary and description (or a mix of both). ICS files containing multiple calendars are supported.

## Getting started
Firstly you need to configure the program according to your needs. The provided example configuration is commented and more details about rules are given below.

The recommended method to use this is with the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) tool:
```shell
# Replace config.yaml with the path to your config file
stack run -- config.yaml
```

Alternatively you could probably use [Cabal](https://www.haskell.org/cabal/), however this has not been tested.

## Example rules:

You can have an unlimited number of rules, and each can be one of 2 types, `uid` and `search`. For example:
```yaml
rules:
- uid: 00000000-0000-0000-0000-000000000000
- uid: 00000000-0000-0000-0000-000000000001
- search: Event One
- search: Event Two
```
This would delete any events with either of the UIDs specified (note this matching is exact), and would also delete any events where the summary or description contained at least one of "Event One" and "Event Two" (case sensitive)
