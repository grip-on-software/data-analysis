SELECT sprint_prev_devs.project_id, sprint_prev_devs.sprint_id, SUM(sprint_prev_devs.num_prev_sprints) AS dev_sprint_experience
FROM (
    SELECT sprint_prev_dev.project_id, sprint_prev_dev.sprint_id, sprint_prev_dev.developer_id, COUNT(*) AS num_prev_sprints
    FROM (
        SELECT DISTINCT
            commits.project_id, commits.sprint_id,
            commits2.sprint_id AS previous_sprint_id,
            CASE WHEN vcs_developer.jira_dev_id IS NOT NULL AND vcs_developer.jira_dev_id <> 0
                THEN vcs_developer.jira_dev_id
                ELSE -commits.developer_id
            END AS developer_id
        FROM (gros.commits
            LEFT JOIN gros.vcs_developer ON commits.developer_id = vcs_developer.alias_id
        )
        JOIN (gros.commits AS commits2
            LEFT JOIN gros.vcs_developer AS developer2 ON commits2.developer_id = developer2.alias_id
        )
        ON commits2.project_id = commits.project_id
        AND (commits.developer_id = commits2.developer_id OR vcs_developer.alias_id = developer2.alias_id)
        WHERE commits2.sprint_id < commits.sprint_id
        AND commits.sprint_id <> 0 AND commits2.sprint_id <> 0
    ) AS sprint_prev_dev
    GROUP BY sprint_prev_dev.project_id, sprint_prev_dev.sprint_id, sprint_prev_dev.developer_id
) AS sprint_prev_devs
GROUP BY sprint_prev_devs.project_id, sprint_prev_devs.sprint_id
ORDER BY project_id, sprint_id
