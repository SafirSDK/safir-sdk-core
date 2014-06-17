/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
* 
* Created by: Joel Ottosson / stjoot
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

using System;
using System.Xml;
using System.Xml.Serialization;
using System.IO;

namespace Sate
{
    /// <summary>
    /// Summary description for Settings.
    /// </summary>
    ///

    // Common base class for SubInfo, SubRegInfo, RegInfo to make scenarios easier
    [XmlInclude(typeof(EntityIdInfo))]
    [XmlInclude(typeof(SubInfo))]
    [XmlInclude(typeof(SubRegInfo))]   
    [XmlInclude(typeof(RegInfo))]
    abstract public class ScenarioInfo
    {
        public ScenarioInfo()
        {
        }
    }
   
    public class EntityIdInfo : ScenarioInfo
    {
        public EntityIdInfo()
        {
        }
    
        //public Safir.Dob.Typesystem.EntityId entityId;
        public EntityIdSerializeable entityIdSer;
    }

    public class SubInfo : ScenarioInfo
    {
        public SubInfo()
        {
        }
        
        public SubInfo(Int64 typeId, EntityIdSerializeable entityIdSer, ChannelIdSerializable channelIdSer, bool upd, bool includeSubClasses, bool restartSubscription)
        {
            this.typeId = typeId;
            this.entityIdSer = entityIdSer;
            this.channelIdSer = channelIdSer;
            this.upd = upd;
            this.includeSubClasses = includeSubClasses;
            this.restartSubscription = restartSubscription;
        }
        
        public Int64 typeId;
        public EntityIdSerializeable entityIdSer;
        public ChannelIdSerializable channelIdSer;
        public bool upd, includeSubClasses, restartSubscription;
    }

    public class SubRegInfo : ScenarioInfo
    {
        public SubRegInfo()
        {
        }
        
        public SubRegInfo(Int64 typeId, HandlerIdSerializeable handlerIdSer, bool includeSubClasses, bool restartSubscription)
        {
            this.typeId = typeId;
            this.handlerIdSer = handlerIdSer;
            this.includeSubClasses = includeSubClasses;
            this.restartSubscription = restartSubscription;
        }
        
        public Int64 typeId;
        public HandlerIdSerializeable handlerIdSer;
        public bool includeSubClasses;
        public bool restartSubscription;
    }

    public class RegInfo : ScenarioInfo
    {
        public RegInfo()
        {
        }
        
        public RegInfo(Int64 typeId, HandlerIdSerializeable handlerIdSer, bool pending, bool injection, bool requestorDecides)
        {
            this.typeId = typeId;
            this.handlerIdSer = handlerIdSer;
            this.pending = pending;
            this.injection = injection;
            this.requestorDecides = requestorDecides;
        }

        public Int64 typeId;
        public HandlerIdSerializeable handlerIdSer;
        public bool pending;
        public bool injection;
        public bool requestorDecides;
    }


    public class ImageMapping
    {
        public string MapObject;
        public string ImagePath;
        public bool Inherit;

        public ImageMapping()
        {
        }

        public ImageMapping(string dobUnit, string path, bool inherit)
        {
            MapObject = dobUnit;
            ImagePath = path;
            Inherit = inherit;
        }
    }

    [XmlRootAttribute("SateSettings", Namespace="", IsNullable = false)]
    public class SateSettings
    {
        public string ConnectionName;
        public bool ConnectAtStartUp;
        public int InboxQueueuLength;
        public bool NoDispatch;
        public bool NoResponse;
        public bool AutoCreate;
        public bool AutoUpdate;
        public bool AutoDelete;
        public bool AutoCreatePersistent;
        public string XmlReplyObject;
        public bool DefaultExplorerView; //true=inheritance false=namespaces
        public RegInfo[] Register;
        public SubInfo[] Subscribe;
        public SubRegInfo[] SubscribeReg;
        public ImageMapping[] ImageMappings;
       
        [XmlIgnore]
        public Safir.Dob.Response AutoResponse;

        public SateSettings()
        {
            ConnectionName="SATE";
            ConnectAtStartUp=true;
            InboxQueueuLength=10;
            NoDispatch = false;
            NoResponse = false;
            AutoCreate=true;
            AutoUpdate=true;
            AutoDelete=true;
            AutoCreatePersistent = true;
            XmlReplyObject = null;
            DefaultExplorerView=true;
            Register=new RegInfo[0];
            Subscribe = new SubInfo[0];
            SubscribeReg = new SubRegInfo[0];
            ImageMappings = new ImageMapping[0];
            AutoResponse = null;
        }

        public void AddSubscription(SubInfo si)
        {
            SubInfo[] tmp = new SubInfo[Subscribe.Length + 1];
            for (int i = 0; i < Subscribe.Length; i++)
            {
                if (Subscribe[i].typeId == si.typeId && Subscribe[i].channelIdSer.ChannelId() == si.channelIdSer.ChannelId())
                {
                    if (si.upd) Subscribe[i].upd = true;
                    return;
                }
                tmp[i] = Subscribe[i];
            }
            tmp[tmp.Length - 1] = si;
            Subscribe = tmp;
        }

        public void AddSubscriptionReg(SubRegInfo sri)
        {
            SubRegInfo[] tmp = new SubRegInfo[Subscribe.Length + 1];
            for (int i = 0; i < SubscribeReg.Length; i++)
            {
                if (SubscribeReg[i].typeId == sri.typeId && SubscribeReg[i].handlerIdSer.HandlerId() == sri.handlerIdSer.HandlerId())
                {
                    return;
                }
                tmp[i] = SubscribeReg[i];
            }
            tmp[tmp.Length - 1] = sri;
            SubscribeReg = tmp;
        }

        public void AddRegistration(RegInfo ri)
        {
            RegInfo[] tmp = new RegInfo[Register.Length + 1];
            for (int i = 0; i < Register.Length; i++)
            {
                if (Register[i].typeId == ri.typeId && Register[i].handlerIdSer.HandlerId() == ri.handlerIdSer.HandlerId())
                {
                    if (ri.pending) Register[i].pending = true;
                    if (ri.injection) Register[i].injection = true;
                    return;
                }
                tmp[i] = Register[i];
            }
            tmp[tmp.Length - 1] = ri;
            Register = tmp;
        }

        public void AddImageMapping(ImageMapping imageMapping)
        {
            //find if mapping already exists
            for (int i = 0; i < ImageMappings.Length; i++)
            {
                if (ImageMappings[i].MapObject == imageMapping.MapObject)
                {
                    ImageMappings[i] = imageMapping;
                    return;
                }
            }

            //no previous mapping existed, append new mapping
            ImageMapping[] im = new ImageMapping[ImageMappings.Length + 1];
            for (int i = 0; i < ImageMappings.Length; i++)
            {
                im[i] = ImageMappings[i];
            }
            im[im.Length - 1] = imageMapping;
            ImageMappings = im;
        }

        public void RemoveImageMapping(string mapObject)
        {
            System.Collections.Generic.List<ImageMapping> list = new System.Collections.Generic.List<ImageMapping>();

            //collect remaining mappings
            for (int i = 0; i < ImageMappings.Length; i++)
            {
                if (ImageMappings[i].MapObject != mapObject)
                {
                    list.Add(ImageMappings[i]);
                }
            }

            //convert to plain array
            ImageMapping[] tmp = new ImageMapping[list.Count];
            for (int i = 0; i < tmp.Length; i++)
            {
                tmp[i]=list[i];
            }

            ImageMappings=tmp;
        }

        public void RemoveImageAndMappings(string imagePath)
        {
            System.Collections.Generic.List<ImageMapping> list = new System.Collections.Generic.List<ImageMapping>();

            //collect remaining mappings
            for (int i = 0; i < ImageMappings.Length; i++)
            {
                if (ImageMappings[i].ImagePath != imagePath)
                {
                    list.Add(ImageMappings[i]);
                }
            }

            //convert to plain array
            ImageMapping[] tmp = new ImageMapping[list.Count];
            for (int i = 0; i < tmp.Length; i++)
            {
                tmp[i] = list[i];
            }

            ImageMappings = tmp;
        }
    }


    public class Settings
    {
        private static string FILE = System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                                                            "SateSettings.xml");
        private static SateSettings settings = null;

        public static void Load()
        {
            XmlSerializer serializer = new XmlSerializer(typeof(SateSettings));
          //  XmlSerializer serializer = new XmlSerializer(typeof(ChannelIdSerializable));
         
            try
            {
                if (File.Exists(FILE))
                {
                    using (FileStream fs = new FileStream(FILE, FileMode.Open))
                    {
                        settings = (SateSettings)serializer.Deserialize(fs);
                        fs.Close();
                    }
                }
                else
                {
                    settings = new SateSettings();
                }
            }
            catch
            {
                System.Windows.Forms.MessageBox.Show("The file SateSettings.xml is corrupt or obsolete. It will be replaced with a new file containing default settings.", "Invalid Settings");
                settings = new SateSettings();
                Save();
            }
        }

        public static void Save()
        {
            XmlSerializer serializer = new XmlSerializer(typeof(SateSettings));
            using (TextWriter writer = new StreamWriter(FILE))
            {
                serializer.Serialize(writer, settings);
            }
        }

        public static SateSettings Sate
        {
            get { return settings; }
            set { settings=value; }
        }
    }
}
