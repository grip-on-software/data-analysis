SELECT switched_issue.project_id, num_switched_stories/CAST(num_total_stories AS double)*100 AS switched_ratio
FROM (SELECT project_id, COUNT(*) AS num_switched_stories FROM (
    SELECT project_id, issue_id, COUNT(*) FROM (
        SELECT DISTINCT issue.project_id, issue.issue_id, sprint.sprint_id
        FROM gros.issue JOIN gros.sprint ON issue.sprint_id = sprint.sprint_id
        WHERE sprint.sprint_id IS NOT NULL AND type = 7
        AND status = 3
    ) AS issue2 GROUP BY project_id, issue_id HAVING COUNT(*) > 1
) AS issue3 GROUP BY project_id) AS switched_issue
JOIN
(SELECT project_id, COUNT(*) AS num_total_stories FROM (
    SELECT project_id, issue_id, COUNT(*) FROM (
        SELECT DISTINCT issue.project_id, issue.issue_id, sprint.sprint_id
        FROM gros.issue JOIN gros.sprint ON issue.sprint_id = sprint.sprint_id
        WHERE sprint.sprint_id IS NOT NULL AND type = 7
        AND status = 3
    ) AS all_issue2 GROUP BY project_id, issue_id
) AS all_issue3 GROUP BY project_id) AS all_issue 
ON switched_issue.project_id = all_issue.project_id;
