SELECT ${s(project_name)} AS project_name, ${f(join_cols, "sprint", mask=2, alias=F)} AS sprint_id, ${t("sprint")}."name" AS sprint_name, ${t("commits")}.commit_date AS date
FROM gros.${t("commits")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "commits", "sprint")}
JOIN gros.${t("project")} ON ${j(join_cols, "commits", "project", mask=1)}
