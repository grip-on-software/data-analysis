SELECT EXTRACT(day FROM issue.updated - sprint.start_date) AS sprint_day, EXTRACT(hour FROM issue.updated) AS sprint_hour, COUNT(*) AS frequency
FROM gros.issue, gros.issue AS prev_issue, gros.sprint
WHERE issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
AND issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
AND issue.story_points <> prev_issue.story_points AND issue.updated < sprint.end_date
GROUP BY sprint_day, sprint_hour HAVING COUNT(*) > 9
ORDER BY sprint_day, sprint_hour;
