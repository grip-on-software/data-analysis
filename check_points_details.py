"""
Script to check the features for backlog points on detail level.

Copyright 2017-2020 ICTU
Copyright 2017-2022 Leiden University

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""

from argparse import ArgumentParser
import json

def parse_args():
    """
    Parse command line arguments.
    """

    parser = ArgumentParser(description='Check details points formula')
    parser.add_argument('details', help='Path to the details.json file')
    return parser.parse_args()

def total(details, key, sprint, story_filter=lambda x: True):
    """
    Calculate the total number of points from details of a feature key for
    a specific sprint, possibly filtered.
    """

    return sum(x if x is not None and story_filter(x) else 0
               for x in details[key].get(sprint, {}).get("story_points", []))

def keys(details, key, sprint, story_filter=lambda x: True):
    """
    Retrieve the issue keys for stories involved in the details of a feature
    key for a specific sprint, possibly filtered.
    """

    sprint_details = details[key].get(sprint, {})
    return {x for x, y in zip(sprint_details.get("key", []), sprint_details.get("story_points", []))
            if y is not None and story_filter(y)}

def main():
    """
    Main entry point.
    """

    with open(parse_args().details) as details_file:
        details = json.load(details_file)

    sprints = list(details["done_story_points"].keys())
    for prev, cur in zip(sprints[:-1], sprints[1:]):
        prev_remain = total(details, "backlog_story_points", prev)
        under = -total(details, "backlog_modified_story_points", prev, lambda x: x > 0)
        added = total(details, "backlog_added_story_points", prev)

        complete = total(details, "done_story_points", cur)
        discard = total(details, "backlog_removed_story_points", cur)
        over = total(details, "backlog_modified_story_points", cur, lambda x: x < 0)
        remain = total(details, "backlog_story_points", cur)

        if prev_remain + under + added != complete + discard + over + remain:
            print(prev, cur, prev_remain + under + added, complete + discard + over + remain)
            #prev_keys = keys(details, "backlog_story_points", prev)
            #under_keys = keys(details, "backlog_modified_story_points", prev, lambda x: x > 0)
            #added_keys = keys(details, "backlog_added_story_points", prev)

            complete_keys = keys(details, "done_story_points", cur)
            discard_keys = keys(details, "backlog_removed_story_points", cur)
            over_keys = keys(details, "backlog_modified_story_points", cur, lambda x: x < 0)
            #remain_keys = keys(details, "backlog_story_points", cur)

            #print(prev_keys & under_keys)
            #print(prev_keys & added_keys)
            #print(under_keys & added_keys)

            print(complete_keys & discard_keys)
            print(complete_keys & over_keys)
            #print(complete_keys & remain_keys)
            print(discard_keys & over_keys)
            #print(discard_keys & remain_keys)
            #print(over_keys & remain_keys)

    # Latest sprint jira backlog comparison?

if __name__ == "__main__":
    main()
