-- Number of previous sprints that all developers have done in the project
SELECT ${f(join_cols, "sprint_prev_devs")}, SUM(sprint_prev_devs.num_prev_sprints) AS dev_sprint_experience
FROM (
    SELECT ${f(join_cols, "sprint_prev_dev")}, sprint_prev_dev.developer_id, COUNT(*) AS num_prev_sprints
    FROM (
        SELECT DISTINCT
            ${f(join_cols, "commits1")},
            commits2.sprint_id AS previous_sprint_id,
            CASE WHEN vcs_developer.jira_dev_id IS NOT NULL AND vcs_developer.jira_dev_id > 0
                THEN vcs_developer.jira_dev_id
                ELSE -commits1.developer_id
            END AS developer_id
        FROM
            (SELECT DISTINCT ${f(join_cols, "commits")}, commits.developer_id
            FROM gros.commits) AS commits1
            LEFT JOIN gros.vcs_developer
			ON commits1.developer_id = vcs_developer.alias_id

        JOIN (
            (SELECT DISTINCT
                ${f(join_cols, "commits")}, commits.developer_id
            FROM gros.commits)) AS commits2
            LEFT JOIN gros.vcs_developer AS developer2
			ON commits2.developer_id = developer2.alias_id

        ON ${j(join_cols[1], "commits2", "commits1")}
        AND (commits1.developer_id = -commits2.developer_id OR vcs_developer.alias_id = developer2.alias_id)
        AND commits2.sprint_id < commits1.sprint_id
        WHERE commits1.sprint_id <> 0 AND commits2.sprint_id <> 0
        AND vcs_developer.jira_dev_id <> -1
    ) AS sprint_prev_dev
    GROUP BY ${f(join_cols, "sprint_prev_dev")}, sprint_prev_dev.developer_id
) AS sprint_prev_devs
GROUP BY ${f(join_cols, "sprint_prev_devs")}
