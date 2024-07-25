SELECT new_developer.project_id, new_developer.sprint_id, COUNT(*) AS num_new_developers FROM (
    SELECT DISTINCT
        commits.project_id, commits.sprint_id,
        CASE WHEN vcs_developer.jira_dev_id IS NOT NULL AND vcs_developer.jira_dev_id <> 0
            THEN vcs_developer.jira_dev_id
            ELSE -commits.developer_id
        END AS developer_id
    FROM (gros.commits
        LEFT JOIN gros.vcs_developer ON commits.developer_id = vcs_developer.alias_id
    )
    LEFT JOIN (gros.commits AS commits2
        LEFT JOIN gros.vcs_developer AS developer2 ON commits2.developer_id = developer2.alias_id
    )
    ON commits2.project_id = commits.project_id
    AND (commits.developer_id = commits2.developer_id OR vcs_developer.alias_id = developer2.alias_id)
    AND commits2.sprint_id < commits.sprint_id
    AND commits2.sprint_id <> 0
    WHERE commits.sprint_id <> 0 AND commits2.sprint_id IS NULL
) AS new_developer
GROUP BY new_developer.project_id, new_developer.sprint_id
