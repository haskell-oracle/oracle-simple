alter session set "_oracle_script"=true;
CREATE USER username IDENTIFIED BY password;
GRANT ALL PRIVILEGES TO username;
GRANT EXECUTE ON DBMS_AQADM TO username;