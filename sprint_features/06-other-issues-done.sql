SELECT other_issue.project_id, other_issue.sprint_id, COUNT(*) AS other_done_issues FROM
    (SELECT issue.project_id, issue.sprint_id, issue.issue_id FROM gros.issue
    WHERE ${issue_other}
    AND issue.sprint_id <> 0
    AND ${issue_done}
    GROUP BY issue.project_id, issue.sprint_id, issue.issue_id) AS other_issue
GROUP BY other_issue.project_id, other_issue.sprint_id
