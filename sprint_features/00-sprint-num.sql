-- Sprint sequence number within project
SELECT ${f(join_cols, "sprint")}, COUNT(*) AS sprint_num
FROM (gros.${t("sprint")} AS sprint1
JOIN gros.project ON ${j(join_cols, "sprint1", "project", 1)}
LEFT JOIN gros.${t("sprint")} AS sprint2
ON ${j(join_cols, "sprint1", "sprint2", 1)}
${s(component_join, project="sprint1")}
WHERE sprint1.project_id = sprint2.project_id
--AND ${s(sprint_open)} >= sprint2.start_date
AND sprint1.sprint_id >= sprint2.sprint_id
${s(sprint_conditions, sprint='sprint1')}
${s(sprint_conditions, sprint='sprint2')}
${g(join_cols, "sprint")}
