DROP TABLE ThumperTable;

CREATE TABLE ThumperTable(
    Timestamp_Count serial primary key, 
    IP_Address inet,
    Version smallint,
    Policy char(128),
    Hash_Algorithm char(128),
    Hash_Message int,
    Serial_Number bigint,
    Generalized_Time char(15)
);

/*
INSERT INTO ThumperTable (IP_Address, Version, Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time)VALUES
    ('193.91.1.17', 1, '0.90.192.12.1.23.0.0.1.34', '2.19.293.28.0.0.23.1.3.24.98.19.0', 255, 9223372036854775805, '202203311440194'),
    ('192.0.8.20', 1, '1.3.6.14.311.5.1007', '0.14.0.0.98.19.0.293.28.0.0.23.1.3.2', 65, 72036854775805, '201712041218598'),
    ('193.89.1.77', 1, '1.3.6.14.31.0.1.5.10.0.7', '1.0.0.98.19.0.0.1.0.0.23.1.3.2', 3, 11172890368547758, '191702282359590'),
    ('193.89.1.77', 1, '1.3.1.0.6.14.3.1.5.10.0.7', '2.19.0.0.1.0.0.23.1.3.2.0', 150, 811368890754717258, '20250611813390')
    ;
*/

