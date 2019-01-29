-- Sprint sequence number within project
SELECT ${f(join_cols, "sprint")}, COUNT(*) AS sprint_num
FROM (gros.${t("sprint")} AS sprint1
JOIN gros.project ON ${j(join_cols, "sprint1", "project", mask=1)}
LEFT JOIN gros.${t("sprint")} AS sprint2
ON ${j(join_cols, "sprint1", "sprint2", mask=1)}
${s(component_join, project="sprint1")}
WHERE ${j(join_cols, "sprint1", "sprint2", mask=1)}
--AND ${s(sprint_open)} >= sprint2.start_date
AND ${s(sprint_id, sprint="sprint1")} >= ${s(sprint_id, sprint="sprint2")}
${s(sprint_conditions, sprint="sprint1")}
${s(sprint_conditions, sprint="sprint2")}
${s(project_condition, project="sprint1")}
${g(join_cols, "sprint")}
