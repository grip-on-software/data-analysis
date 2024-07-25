-- metric_version
ALTER TABLE gros.metric_version ADD COLUMN temp_version_id VARCHAR(100);
UPDATE gros.metric_version SET temp_version_id=CAST(version_id AS VARCHAR(100));
ALTER TABLE gros.metric_version DROP CONSTRAINT "pk_metric_version_id";
ALTER TABLE gros.metric_version DROP COLUMN version_id;

ALTER TABLE gros.metric_version ADD COLUMN version_id VARCHAR(100);
UPDATE gros.metric_version SET version_id=temp_version_id;
ALTER TABLE gros.metric_version DROP COLUMN temp_version_id;
ALTER TABLE gros.metric_version ALTER COLUMN version_id SET NOT NULL;
ALTER TABLE gros.metric_version ADD CONSTRAINT "pk_metric_version_id" PRIMARY KEY ("project_id", "version_id");

-- metric_target
ALTER TABLE gros.metric_target ADD COLUMN temp_version_id VARCHAR(100);
UPDATE gros.metric_target SET temp_version_id=CAST(version_id AS VARCHAR(100));
ALTER TABLE gros.metric_target DROP COLUMN version_id;

ALTER TABLE gros.metric_target ADD COLUMN version_id VARCHAR(100);
UPDATE gros.metric_target SET version_id=temp_version_id;
ALTER TABLE gros.metric_target DROP COLUMN temp_version_id;