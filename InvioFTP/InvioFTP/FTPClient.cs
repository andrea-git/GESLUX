using System;
using System.IO;
using System.Text;
using System.Net;
using System.Security.Cryptography.X509Certificates;
using System.Net.Security;

namespace InvioFTP
{
    
    public class FTPClient
    {
        public FTPClient()
        {

            //Iscrivo la mia callback per il certificato
            ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(FTPClient.ValidateServerCertificate);
            ServicePointManager.Expect100Continue = true;
        }

        public static bool ValidateServerCertificate(
                            object sender,
                            X509Certificate certificate,
                            X509Chain chain,
                            SslPolicyErrors sslPolicyErrors)
        {
            if (sslPolicyErrors == SslPolicyErrors.None)
                return true;

            Console.WriteLine("Certificate error: {0}", sslPolicyErrors);

            return false;
        }

        public string UploadFTP(string FTPServer, string FtpFile, string FtpLocalPath, string FtpUser, string FtpPass)
        {
            string StatusFtp = "";    
        try
        {
                string PathToUpload = FTPServer + FtpFile;
                // Get the object used to communicate with the server.
                FtpWebRequest request = (FtpWebRequest)WebRequest.Create(PathToUpload);
            
                request.KeepAlive = false;
                request.UseBinary = true;
                request.UsePassive = true;
                request.Timeout = -1;
                request.EnableSsl = true;
                request.Proxy = null;

                request.Method = WebRequestMethods.Ftp.UploadFile;
                request.Credentials = new NetworkCredential(FtpUser, FtpPass);

                // Copy the contents of the file to the request stream.
                string FileToUpload = FtpLocalPath + FtpFile;
                StreamReader sourceStream = new StreamReader(FileToUpload);
                byte[] fileContents = Encoding.ASCII.GetBytes(sourceStream.ReadToEnd());
                sourceStream.Close();
                request.ContentLength = fileContents.Length;
                Stream requestStream = request.GetRequestStream();
                requestStream.Write(fileContents, 0, fileContents.Length);
                requestStream.WriteTimeout = -1;
                requestStream.Close();
                FtpWebResponse response = (FtpWebResponse)request.GetResponse();
                StatusFtp = (response.StatusDescription);
                response.Close();
            }
            catch (Exception ex)
            {
                StatusFtp = ex.ToString();
                
            }

            
            return StatusFtp;
        }
    }
}
