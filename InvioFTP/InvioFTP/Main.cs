using System;
using System.IO;
using System.Net;
using System.Text;
using System.Reflection;
using WinSCP;

namespace InvioFTP
{
    public class Inizio
    {
        public static void Main()
        {
            int i = 0;
            string _hostName = "";
            int _port = 0;
            string _statusFtp1 = "";
            string _userName = "";
            string _password = "";
            string _fTPFile1 = "";
            string _fTPFile2 = "";
            string _localPath = "";
            string line = "";
            string _fileIni = "";
            string _sslHostCertificateFingerprint = ""; /*"af:ab:e4:30:31:51:56:47:7d:69:86:9c:eb:38:bb:98:9d:43:03:02"*/

            string _statusFtp2 = "";
            string _csRemoteDirectory = "";

            _fileIni = Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "");
            FileInfo info = new FileInfo(_fileIni);
            _fileIni = info.DirectoryName + "\\ini\\InvioFTP.ini";

            StreamReader FileIni = new StreamReader(_fileIni);

            while ((line = FileIni.ReadLine()) != null)
            {
                i++;

                switch (i)
                {
                    case 1:
                        _hostName = line;
                        break;
                    case 2:
                        _port = Convert.ToInt16(line);
                        break;
                    case 3:
                        _userName = line;
                        break;
                    case 4:
                        _password = line;
                        break;
                    case 5:
                        _csRemoteDirectory = line;
                        break;
                    case 6:
                        _fTPFile1 = line;
                        break;
                    case 7:
                        _fTPFile2 = line;
                        break;
                    case 8:
                        _localPath = @line;
                        break;
                    case 9:
                        _sslHostCertificateFingerprint = line;
                        break;
                    default:
                        break;
                }


            }

            FileIni.Close();

            // Setup session options
            SessionOptions sessionOptionsNormal = new SessionOptions
            {
                Protocol = WinSCP.Protocol.Ftp,
                FtpMode = FtpMode.Passive,
                FtpSecure = FtpSecure.ExplicitTls, //.Implicit,
                GiveUpSecurityAndAcceptAnySslHostCertificate = true,
                PortNumber = _port, //990,
                SslHostCertificateFingerprint = _sslHostCertificateFingerprint,

                HostName = _hostName,
                UserName = _userName,
                Password = _password,
                //TlsHostCertificateFingerprint = "ssh-rsa 2048 98:40:cf:38:04:7b:61:54:7f:fc:d3:06:31:b9:08:95",
                //SshHostKeyFingerprint = "ssh-rsa 2048 98:40:cf:38:04:7b:61:54:7f:fc:d3:06:31:b9:08:95",
                //SslHostCertificateFingerprint = "af:ab:e4:30:31:51:56:47:7d:69:86:9c:eb:38:bb:98:9d:43:03:02",

            };

            // Setup session options
            SessionOptions sessionOptionsSecure = new SessionOptions
            {
                Protocol = WinSCP.Protocol.Ftp,
                FtpMode = FtpMode.Passive,
                FtpSecure = FtpSecure.Implicit,
                GiveUpSecurityAndAcceptAnySslHostCertificate = true,
                PortNumber = _port, //990,
                SslHostCertificateFingerprint = _sslHostCertificateFingerprint,

                HostName = _hostName,
                UserName = _userName,
                Password = _password,
                //TlsHostCertificateFingerprint = "ssh-rsa 2048 98:40:cf:38:04:7b:61:54:7f:fc:d3:06:31:b9:08:95",
                //SshHostKeyFingerprint = "ssh-rsa 2048 98:40:cf:38:04:7b:61:54:7f:fc:d3:06:31:b9:08:95",
                //SslHostCertificateFingerprint = "af:ab:e4:30:31:51:56:47:7d:69:86:9c:eb:38:bb:98:9d:43:03:02",

            };

            //FARE PRIMA UNA COPIA
            using (Session session = new Session())
            {
                try
                {

                    // Connect
                    if (_port == 21)
                    {
                        session.Open(sessionOptionsNormal);
                    }
                    else
                    {
                        session.Open(sessionOptionsSecure);
                    }

                    // Upload files
                    TransferOptions transferOptions = new TransferOptions();
                    transferOptions.TransferMode = TransferMode.Binary;

                    TransferOperationResult transferResult;
                    string _fullFileName1 = @_localPath + _fTPFile1;
                    BackupFile(_fullFileName1);
                    transferResult = session.PutFiles(_fullFileName1, _csRemoteDirectory, false, transferOptions);

                    // Throw on any error
                    transferResult.Check();

                    // Print results
                    foreach (TransferEventArgs transfer in transferResult.Transfers)
                    {
                        _statusFtp1 = "Upload of " + transfer.FileName + " succeeded";
                    }

                    string _fullFileName2 = @_localPath + _fTPFile2;
                    BackupFile(_fullFileName2);
                    transferResult = session.PutFiles(_fullFileName2, _csRemoteDirectory, false, transferOptions);

                    // Throw on any error
                    transferResult.Check();

                    // Print results
                    foreach (TransferEventArgs transfer in transferResult.Transfers)
                    {
                        _statusFtp2 = "Upload of " + transfer.FileName + " succeeded";
                    }

                }
                catch (Exception ex)
                {
                    _statusFtp1 = ex.ToString();

                }
            }
            WriteLog(_fileIni, _statusFtp1, _statusFtp2);
            BackupFile(_fileIni);
        }
        
        public static void BackupFile(string FileToBackup)
        {

            string _extension = Path.GetExtension(FileToBackup);

            string _fileBackup = Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "");
            FileInfo info = new FileInfo(_fileBackup);

            if (_extension == ".ini")
            {
                DateTime oggi = DateTime.Now;
                _fileBackup = info.DirectoryName + "\\backup\\InvioFTP_" + oggi.ToString("yyyyMMdd-HHmmss") + ".ini";
            }
            else 
            {
                string _fileName = Path.GetFileName(FileToBackup);
                _fileBackup = info.DirectoryName + "\\backup\\" + _fileName;
            }
            File.Copy(FileToBackup, _fileBackup, true);


        }

        public static void WriteLog(string FileToWrite, string StatusFtp1, string StatusFtp2)
        {
            StreamWriter FileLog = new StreamWriter(FileToWrite, true);

            FileLog.WriteLine("");
            FileLog.WriteLine("");
            FileLog.WriteLine("RISULTATO DELL'ELABORAZIONE:");
            FileLog.WriteLine("");

            FileLog.WriteLine("*** " + StatusFtp1);
            FileLog.WriteLine("*** " + StatusFtp2);
            FileLog.Close();

        }


   }
}