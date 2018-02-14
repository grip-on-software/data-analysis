SELECT
    issue.project_id, issue.sprint_id,
SUM(issue.story_points) AS num_points,
    SUM(CAST(
        CAST(issue.story_points AS double) / 
        CAST(
            CASE WHEN sprint_points.max_points IS NOT NULL
            AND sprint_points.max_points <> 0
            THEN sprint_points.max_points
            ELSE 1
            END
        AS double)
    AS DECIMAL(5,2))) AS num_weighted_points,
    COUNT(*) AS num_done 
FROM gros.issue,
(SELECT
    issue_id, MIN(changelog_id) AS first_changelog_id
    FROM gros.issue
    WHERE issue.type IN (5,7) --${issue_story}
        AND issue.story_points <> 0
        AND issue.sprint_id <> 0
        AND issue.status = 3 -- issue was in progress during this sprint
    GROUP BY issue_id
) AS points_data,
(SELECT
    issue.issue_id, MIN(sprint.sprint_id) AS done_sprint
    FROM gros.issue, gros.sprint
    WHERE issue.sprint_id = sprint.sprint_id
    AND (issue.resolution = 1 or issue.status = 6)--${issue_done}
    GROUP BY issue.issue_id
) AS done_data,
-- TODO: Weight the maximum points over the last three sprints
(SELECT
    issue.sprint_id, MAX(issue.story_points) AS max_points FROM gros.issue
    --WHERE issue.story_points <= sprint_points_normalization
    GROUP BY issue.sprint_id
) AS sprint_points
WHERE issue.issue_id = points_data.issue_id
AND issue.changelog_id = points_data.first_changelog_id
AND issue.issue_id = done_data.issue_id
AND issue.sprint_id = done_data.done_sprint
AND issue.sprint_id = sprint_points.sprint_id
GROUP BY issue.project_id, issue.sprint_id
ORDER BY num_weighted_points
