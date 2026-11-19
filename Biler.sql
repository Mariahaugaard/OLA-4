
USE Biler;

CREATE TABLE forhandlere (
    cvr VARCHAR(20) PRIMARY KEY,
    forhandler VARCHAR(255)
);

CREATE TABLE biler (
    carid VARCHAR(100) PRIMARY KEY,
    maerke VARCHAR(100),
    model VARCHAR(100),
    reg_dato DATE,
    adresse VARCHAR(255),
    link TEXT,
    forhandler_id VARCHAR(20),
    FOREIGN KEY (forhandler_id)
        REFERENCES forhandlere(cvr)
);

CREATE TABLE prishistorik (
    carid VARCHAR(100),
    scrapedate DATETIME,
    pris_dkk DECIMAL(12,2),
    PRIMARY KEY (carid, scrapedate),
    FOREIGN KEY (carid)
        REFERENCES biler(carid)
);
