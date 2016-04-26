/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using System.Xml.Serialization;
using Safir.Dob;

namespace Sate
{
    /// <summary>
    ///     Summary description for Settings.
    /// </summary>

    // Common base class for SubInfo, SubRegInfo, RegInfo to make scenarios easier
    [XmlInclude(typeof(EntityIdInfo))]
    [XmlInclude(typeof(SubInfo))]
    [XmlInclude(typeof(SubRegInfo))]
    [XmlInclude(typeof(RegInfo))]
    public abstract class ScenarioInfo
    {
    }

    public class EntityIdInfo : ScenarioInfo
    {
        //public Safir.Dob.Typesystem.EntityId entityId;
        public EntityIdSerializeable entityIdSer;
    }

    public class SubInfo : ScenarioInfo
    {
        public ChannelIdSerializable channelIdSer;
        public EntityIdSerializeable entityIdSer;

        public long typeId;
        public bool upd, includeSubClasses, restartSubscription;

        public SubInfo()
        {
        }

        public SubInfo(long typeId, EntityIdSerializeable entityIdSer, ChannelIdSerializable channelIdSer, bool upd,
            bool includeSubClasses, bool restartSubscription)
        {
            this.typeId = typeId;
            this.entityIdSer = entityIdSer;
            this.channelIdSer = channelIdSer;
            this.upd = upd;
            this.includeSubClasses = includeSubClasses;
            this.restartSubscription = restartSubscription;
        }
    }

    public class SubRegInfo : ScenarioInfo
    {
        public HandlerIdSerializeable handlerIdSer;
        public bool includeSubClasses;
        public bool restartSubscription;

        public long typeId;

        public SubRegInfo()
        {
        }

        public SubRegInfo(long typeId, HandlerIdSerializeable handlerIdSer, bool includeSubClasses,
            bool restartSubscription)
        {
            this.typeId = typeId;
            this.handlerIdSer = handlerIdSer;
            this.includeSubClasses = includeSubClasses;
            this.restartSubscription = restartSubscription;
        }
    }

    public class RegInfo : ScenarioInfo
    {
        public HandlerIdSerializeable handlerIdSer;
        public bool injection;
        public bool pending;
        public bool requestorDecides;

        public long typeId;

        public RegInfo()
        {
        }

        public RegInfo(long typeId, HandlerIdSerializeable handlerIdSer, bool pending, bool injection,
            bool requestorDecides)
        {
            this.typeId = typeId;
            this.handlerIdSer = handlerIdSer;
            this.pending = pending;
            this.injection = injection;
            this.requestorDecides = requestorDecides;
        }
    }


    public class ImageMapping
    {
        public string ImagePath;
        public bool Inherit;
        public string MapObject;

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

    [XmlRoot("SateSettings", Namespace = "", IsNullable = false)]
    public class SateSettings
    {
        public bool AutoCreate;
        public bool AutoCreatePersistent;
        public bool AutoDelete;

        [XmlIgnore] public Response AutoResponse;

        public bool AutoUpdate;
        public bool ConnectAtStartUp;
        public string ConnectionName;
        public bool DefaultExplorerView; //true=inheritance false=namespaces
        public ImageMapping[] ImageMappings;
        public int InboxQueueuLength;
        public bool NoDispatch;
        public bool NoResponse;
        public RegInfo[] Register;
        public SubInfo[] Subscribe;
        public SubRegInfo[] SubscribeReg;
        public string XmlReplyObject;

        public SateSettings()
        {
            ConnectionName = "SATE";
            ConnectAtStartUp = true;
            InboxQueueuLength = 10;
            NoDispatch = false;
            NoResponse = false;
            AutoCreate = true;
            AutoUpdate = true;
            AutoDelete = true;
            AutoCreatePersistent = true;
            XmlReplyObject = null;
            DefaultExplorerView = true;
            Register = new RegInfo[0];
            Subscribe = new SubInfo[0];
            SubscribeReg = new SubRegInfo[0];
            ImageMappings = new ImageMapping[0];
            AutoResponse = null;
        }

        public void AddSubscription(SubInfo si)
        {
            var tmp = new SubInfo[Subscribe.Length + 1];
            for (var i = 0; i < Subscribe.Length; i++)
            {
                if (Subscribe[i].typeId == si.typeId &&
                    Subscribe[i].channelIdSer.ChannelId() == si.channelIdSer.ChannelId())
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
            var tmp = new SubRegInfo[Subscribe.Length + 1];
            for (var i = 0; i < SubscribeReg.Length; i++)
            {
                if (SubscribeReg[i].typeId == sri.typeId &&
                    SubscribeReg[i].handlerIdSer.HandlerId() == sri.handlerIdSer.HandlerId())
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
            var tmp = new RegInfo[Register.Length + 1];
            for (var i = 0; i < Register.Length; i++)
            {
                if (Register[i].typeId == ri.typeId &&
                    Register[i].handlerIdSer.HandlerId() == ri.handlerIdSer.HandlerId())
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
            for (var i = 0; i < ImageMappings.Length; i++)
            {
                if (ImageMappings[i].MapObject == imageMapping.MapObject)
                {
                    ImageMappings[i] = imageMapping;
                    return;
                }
            }

            //no previous mapping existed, append new mapping
            var im = new ImageMapping[ImageMappings.Length + 1];
            for (var i = 0; i < ImageMappings.Length; i++)
            {
                im[i] = ImageMappings[i];
            }
            im[im.Length - 1] = imageMapping;
            ImageMappings = im;
        }

        public void RemoveImageMapping(string mapObject)
        {
            var list = new List<ImageMapping>();

            //collect remaining mappings
            for (var i = 0; i < ImageMappings.Length; i++)
            {
                if (ImageMappings[i].MapObject != mapObject)
                {
                    list.Add(ImageMappings[i]);
                }
            }

            //convert to plain array
            var tmp = new ImageMapping[list.Count];
            for (var i = 0; i < tmp.Length; i++)
            {
                tmp[i] = list[i];
            }

            ImageMappings = tmp;
        }

        public void RemoveImageAndMappings(string imagePath)
        {
            var list = new List<ImageMapping>();

            //collect remaining mappings
            for (var i = 0; i < ImageMappings.Length; i++)
            {
                if (ImageMappings[i].ImagePath != imagePath)
                {
                    list.Add(ImageMappings[i]);
                }
            }

            //convert to plain array
            var tmp = new ImageMapping[list.Count];
            for (var i = 0; i < tmp.Length; i++)
            {
                tmp[i] = list[i];
            }

            ImageMappings = tmp;
        }
    }


    public class Settings
    {
        private static readonly string FILE =
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                "SateSettings.xml");

        public static SateSettings Sate { get; set; }

        public static void Load()
        {
            var serializer = new XmlSerializer(typeof(SateSettings));
            //  XmlSerializer serializer = new XmlSerializer(typeof(ChannelIdSerializable));

            try
            {
                if (File.Exists(FILE))
                {
                    using (var fs = new FileStream(FILE, FileMode.Open))
                    {
                        Sate = (SateSettings) serializer.Deserialize(fs);
                        fs.Close();
                    }
                }
                else
                {
                    Sate = new SateSettings();
                }
            }
            catch
            {
                MessageBox.Show(
                    "The file SateSettings.xml is corrupt or obsolete. It will be replaced with a new file containing default settings.",
                    "Invalid Settings");
                Sate = new SateSettings();
                Save();
            }
        }

        public static void Save()
        {
            var serializer = new XmlSerializer(typeof(SateSettings));
            using (TextWriter writer = new StreamWriter(FILE))
            {
                serializer.Serialize(writer, Sate);
            }
        }
    }
}