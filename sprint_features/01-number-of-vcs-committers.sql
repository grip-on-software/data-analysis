-- Number of VCS committers in sprint
SELECT ${f(join_cols, "sprint_devs")}, COUNT(*) AS number_of_vcs_devs FROM (
    SELECT ${f(join_cols, "commits")}, commits.developer_id FROM gros.commits
    GROUP BY ${f(join_cols, "commits")}, commits.developer_id HAVING commits.sprint_id <> 0
) AS sprint_devs
${g(join_cols, "sprint_devs")}
