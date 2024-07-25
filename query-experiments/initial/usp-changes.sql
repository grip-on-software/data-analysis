SELECT project_id, num_changes, COUNT(*) FROM (
    SELECT issue.project_id, issue.issue_id, SUM(CASE WHEN old_issue.issue_id IS NULL THEN 0 ELSE 1 END) AS num_changes
    FROM gros.issue
    LEFT JOIN gros.issue AS old_issue
    ON issue.project_id = old_issue.project_id
    AND issue.issue_id = old_issue.issue_id
    AND issue.changelog_id = old_issue.changelog_id + 1
    WHERE issue.story_points <> old_issue.story_points
    GROUP BY issue.project_id, issue.issue_id
) AS changes GROUP BY project_id, num_changes
