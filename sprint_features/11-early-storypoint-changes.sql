SELECT issue.project_id, max_issue.sprint_id, COUNT(*) AS num_early_storypoint_changes FROM
    gros.issue LEFT JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id + 1,
    (SELECT issue.issue_id, issue.sprint_id, MAX(issue.changelog_id) AS changelog_id
        FROM gros.issue
        GROUP BY issue.issue_id, issue.sprint_id
    ) AS max_issue,
    gros.sprint 
WHERE issue.issue_id = max_issue.issue_id AND issue.changelog_id <= max_issue.changelog_id
AND max_issue.sprint_id = sprint.sprint_id
AND issue.story_points IS NOT NULL
AND (issue.changelog_id = 0 OR issue.story_points <> prev_issue.story_points)
AND issue.updated < ${planned_early}
GROUP BY issue.project_id, max_issue.sprint_id
