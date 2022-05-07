DROP TABLE ThumperTable;

CREATE TABLE ThumperTable(
    Timestamp_Count serial primary key, 
    IP_Address inet,
    Version smallint,
    Policy char(128),
    Hash_Algorithm char(128),
    Hash_Message char(96),  -- the Hash Size is 32 Octets or a (2 * 32) Hexadecimal String with blanks in between = 96
    Serial_Number bigint,
    Generalized_Time char(15)
);

/*
INSERT INTO ThumperTable (IP_Address, Version, Policy, Hash_Algorithm, Hash_Message, Serial_Number, Generalized_Time)VALUES
    ('193.91.1.17', 1, '0.90.192.12.1.23.0.0.1.34', '2.19.293.28.0.0.23.1.3.24.98.19.0', '25 5a f9 0b 39 45 d2 00 2e 80 b3 a5 c6 d7 e8 f9 0a', 9223372036854775805, '202203311440194'),
    ('192.0.8.20', 1, '1.3.6.14.311.5.1007', '0.14.0.0.98.19.0.293.28.0.0.23.1.3.2', '65 f0 ca 04', 72036854775805, '201712041218598'),
    ('193.89.1.77', 1, '1.3.6.14.31.0.1.5.10.0.7', '1.0.0.98.19.0.0.1.0.0.23.1.3.2', '3a b9 30 0b 4b 2d 65 c6 23 11 c1 f1 90 ff ee', 11172890368547758, '191702282359590'),
    ('193.89.1.77', 1, '1.3.1.0.6.14.3.1.5.10.0.7', '2.19.0.0.1.0.0.23.1.3.2.0', 'b1 e3 45 d0', 811368890754717258, '20250611813390')
    ;
*/

