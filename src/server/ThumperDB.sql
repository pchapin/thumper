
DROP TABLE timestamp;

-- A table holding information about every timestamp created.
CREATE TABLE timestamp(
    serial_number    BIGINT PRIMARY KEY,  -- Must be 64 bits.
    client_address   INET,       -- IP address of the client system.
    version          SMALLINT,
    policy           CHAR(128),  -- An OID written in dotted notation.
    hash_algorithm   CHAR(128),  -- An OID written in dotted notation.

    -- The hash size is 32 octets. Written as a hex string with a delimiter between each octet
    -- requires a total of 95 characters (3 * 32) - 1.
    hash             CHAR(95),
    generalized_time CHAR(15)
);


-- Some "fake" data for testing purposes.
INSERT INTO timestamp
  (serial_number, client_address, version, policy, hash_algorithm, hash, generalized_time)
VALUES
  (9223372036854775805,
   '192.168.1.17',
   1,
   '0.90.192.12.1.23.0.0.1.34',
   '2.19.293.28.0.0.23.1.3.24.98.19.0',
   'F5 B2 E3 C8 16 60 4F 28 7D B2 1D 70 34 C5 7B D4 1D 38 62 35 53 31 74 CC 5F FA 48 60 2B 75 2E 03',
   '20220331144019Z'),
   
  (72036854775805,
   '192.168.8.20',
   1,
   '1.3.6.14.311.5.1007',
   '0.14.0.0.98.19.0.293.28.0.0.23.1.3.2',
   '33 4D 01 6F 75 5C D6 DC 58 C5 3A 86 E1 83 88 2F 8E C1 4F 52 FB 05 34 58 87 C8 A5 ED D4 2C 87 B7',
   '20171204121859Z'),
   
  (11172890368547758,
   '192.168.1.77',
   1,
   '1.3.6.14.31.0.1.5.10.0.7',
   '1.0.0.98.19.0.0.1.0.0.23.1.3.2',
   '13 29 C6 F1 E4 5F EB EE 8D 8C ED 53 59 BB E6 CA 3D E4 3D 5A 14 8A 4B 85 8E D9 A2 A3 8A AC E3 A6',
   '19170228235959Z'),
   
  (811368890754717258,
   '155.42.107.97',
   1,
   '1.3.1.0.6.14.3.1.5.10.0.7',
   '2.19.0.0.1.0.0.23.1.3.2.0',
   '95 A2 07 C3 0F D4 F0 3B 86 BF 60 C8 41 2A 74 BF E3 7E 49 0B 62 78 44 7B 63 34 3C CF 67 AC 32 53',
   '2025061181339Z');
