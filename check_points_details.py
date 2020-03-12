from argparse import ArgumentParser
import json

def parse_args():
    parser = ArgumentParser(description='Check details points formula')
    parser.add_argument('details', help='Path to the details.json file')
    return parser.parse_args()

def total(details, key, sprint, filter=lambda x: True):
    return sum(x if x is not None and filter(x) else 0
               for x in details[key].get(sprint, {}).get("story_points", []))

def keys(details, key, sprint, filter=lambda x: True):
    sprint_details = details[key].get(sprint, {})
    return {x for x, y in zip(sprint_details.get("key", []), sprint_details.get("story_points", []))
            if y is not None and filter(y)}

def main():
    args = parse_args()
    with open(args.details) as details_file:
        details = json.load(details_file)

    fields = ["done_story_points", "backlog_added_story_points", "backlog_removed_story_points", "backlog_modified_story_points", "backlog_story_points"]
    sprints = list(details[fields[0]].keys())
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
            prev_keys = keys(details, "backlog_story_points", prev)
            under_keys = keys(details, "backlog_modified_story_points", prev, lambda x: x > 0)
            added_keys = keys(details, "backlog_added_story_points", prev)

            complete_keys = keys(details, "done_story_points", cur)
            discard_keys = keys(details, "backlog_removed_story_points", cur)
            over_keys = keys(details, "backlog_modified_story_points", cur, lambda x: x < 0)
            remain_keys = keys(details, "backlog_story_points", cur)

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
